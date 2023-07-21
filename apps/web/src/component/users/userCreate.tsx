import React, { useState } from 'react';
import Styles from '../../styles/user.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getUsercreationYupschema } from '../../helper/constants/user-constants';
import {
  Grid,
  InputLabel,
  MenuItem,
  Select,
} from '@mui/material';
import { createUser } from '../../hooks/user-hooks';
import { useGetAllRoles } from '../../hooks/userRole-hooks';
import { useNavigate } from 'react-router';
import MySnackbar from '../ui/MySnackbar';
import Input from '../ui/Input';
import Button from '../menu/button';
import { BsFillEyeSlashFill, BsFillEyeFill } from 'react-icons/bs';
import { FaLock } from 'react-icons/fa6';

const validationSchema = getUsercreationYupschema(Yup);
const UserCreate = () => {
  const navigate = useNavigate();
  const [OpenSnackbar, setOpenSnakBar] = useState(false);
  const [isWarning, setIsWarning] = useState(false);
  const [message, setMessage] = useState('');
  const [passwordShown, setPasswordShown] = useState(false);
  const handleSnackBarClose = () => {
    setOpenSnakBar(false);
  };
  const [initialValues, setInitialValues] = useState({
    first_name: '',
    last_name: '',
    username: '',
    user_password: '',
    email_id: '',
    contact_no: '',
    user_status: '',
    role_id: '',
    department:''
  });

  const togglePasswordVisibility = () => {
    setPasswordShown(!passwordShown);
  };

  const { mutate: createNewusers } = createUser();
  const { data: getAllRoles } = useGetAllRoles();
  
  const formik = useFormik({
    initialValues,
    validationSchema,
    onSubmit: (values) => {
      const Object: any = {
        first_name: values.first_name,
        last_name: values.last_name,
        user_password: values.user_password,
        email_id: values.email_id,
        user_status: 'AC',
        contact_no: values.contact_no,
        role_id: values.role_id,
        department:values.department
      };
      console.log("user add==>",Object)
      alert(Object)
      createNewusers(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success) {
            setMessage('User created successfully');
            setOpenSnakBar(true);
            setInterval(() => {
              navigate('/userList');
            }, 3000);
          } else {
            setIsWarning(true);
            setMessage(data?.message);
            setOpenSnakBar(true);
          }
        },
      });
    },
  });
  return (
    <div className={Styles.container}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.fields}>
          <div className={Styles.fieldDiv1}>
            <Grid
              container
              spacing={{ xs: 2, md: 3 }}
              columns={{ xs: 4, sm: 8, md: 12 }}
            >
              <Grid item xs={2} sm={4} md={12}>
                <h2>USER CREATION</h2>
              </Grid>
              <Grid item xs={2} sm={4} md={4}>
                <Input
                  label="First Name"
                  placeholder="Enter first name"
                  name="first_name"
                  value={formik.values.first_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.first_name && formik.errors.first_name
                  }
                  width="80%"
                />
              </Grid>
              <Grid item xs={2} sm={4} md={4}>
                <Input
                  label="Last Name"
                  placeholder="Enter last name"
                  name="last_name"
                  value={formik.values.last_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.last_name && formik.errors.last_name
                  }
                  width="80%"
                />
              </Grid>
              <Grid item xs={2} sm={4} md={4}>
                <Input
                  label="Mobile Number"
                  placeholder="Enter mobile number"
                  name="contact_no"
                  value={formik.values.contact_no}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.contact_no &&
                    formik.errors.contact_no
                  }
                  width="80%"
                />
              </Grid>
              <Grid item xs={2} sm={4} md={4}>
                <Input
                  label="Email"
                  placeholder="Enter email"
                  name="email_id"
                  value={formik.values.email_id}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.email_id && formik.errors.email_id
                  }
                  width="80%"
                />
              </Grid>
              <Grid item xs={2} sm={4} md={4}>
                <Input
                  label="Password"
                  placeholder="Enter password"
                  name="user_password"
                  type={passwordShown ? 'text' : 'password'}
                  value={formik.values.user_password}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.user_password &&
                    formik.errors.user_password
                  }
                  prefixIcon={<FaLock />}
                  suffixIcon={
                    <button
                      type="button"
                      onClick={togglePasswordVisibility}
                      style={{ background: 'none', border: 'none' }}
                    >
                      {passwordShown ? (
                        <BsFillEyeFill size={20} />
                      ) : (
                        <BsFillEyeSlashFill size={20} />
                      )}
                    </button>
                  }
                  width="80%"
                />
              </Grid>
              <Grid item xs={2} sm={4} md={4}>
                <Input
                  label="Department"
                  placeholder="Enter Department"
                  name="department"
                  value={formik.values.department}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.department && formik.errors.department
                  }
                  width="80%"
                />
              </Grid>
              <Grid item xs={2} sm={4} md={6}>
                <InputLabel id="role_id-label">Role</InputLabel>
                <Select
                  labelId="role_id-label"
                  name="role_id"
                  size="small"
                  sx={{ width: '300px' }}
                  value={formik.values.role_id}
                  onChange={formik.handleChange}
                >
                  {getAllRoles &&
                    getAllRoles.map((option: any) => (
                      <MenuItem key={option.role_id} value={option.role_id}>
                        {option.role_name}
                      </MenuItem>
                    ))}
                </Select>
                {formik.errors.role_id && formik.touched.role_id && (
                  <div style={{ color: 'red' }}>{formik.errors.role_id}</div>
                )}
              </Grid>
              <Grid item xs={2} sm={4} md={12}>
                <Button
                  text="Submit"
                  backgroundColor="#7F56D9"
                  fontSize={14}
                  fontWeight={500}
                  width={125}
                />
              </Grid>
              <Grid item xs={2} sm={4} md={4}></Grid>
            </Grid>
          </div>
        </div>
      </form>
      <MySnackbar
        open={OpenSnackbar}
        message={message}
        onClose={handleSnackBarClose}
        severity={isWarning ? 'warning' : 'success'}
        autoHideDuration={1000}
      />
    </div>
  );
};

export default UserCreate;
