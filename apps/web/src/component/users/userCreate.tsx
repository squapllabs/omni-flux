import React, { useState } from 'react';
import Customs from '../ui/custom';
import Styles from '../../styles/user.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getUsercreationYupschema } from '../../helper/constants/user-constants';
import {
  Grid,
  IconButton,
  InputAdornment,
  InputLabel,
  MenuItem,
  Select,
} from '@mui/material';
import { createUser } from '../../hooks/user-hooks';
import { useGetAllRoles } from '../../hooks/userRole-hooks';
import { useNavigate } from 'react-router';
import MySnackbar from '../ui/MySnackbar';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOff from '@mui/icons-material/VisibilityOff';
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
  });

  const handleMouseDownPassword = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.preventDefault();
  };
  const togglePassword = () => {
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
      };
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
                  <Customs.CustomTextField
                    name="first_name"
                    label="First Name"
                    variant="outlined"
                    size="small"
                    value={formik.values.first_name}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.first_name &&
                      Boolean(formik.errors.first_name)
                    }
                    helperText={
                      formik.touched.first_name && formik.errors.first_name
                    }
                  />
                </Grid>
                <Grid item xs={2} sm={4} md={4}>
                  <Customs.CustomTextField
                    name="last_name"
                    label="Last Name"
                    variant="outlined"
                    size="small"
                    value={formik.values.last_name}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.last_name &&
                      Boolean(formik.errors.last_name)
                    }
                    helperText={
                      formik.touched.last_name && formik.errors.last_name
                    }
                  />
                </Grid>
                <Grid item xs={2} sm={4} md={4}>
                  <Customs.CustomTextField
                    name="contact_no"
                    label="Mobile Number"
                    variant="outlined"
                    size="small"
                    value={formik.values.contact_no}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.contact_no &&
                      Boolean(formik.errors.contact_no)
                    }
                    helperText={
                      formik.touched.contact_no && formik.errors.contact_no
                    }
                  />
                </Grid>
                <Grid item xs={2} sm={4} md={4}>
                  <Customs.CustomTextField
                    name="email_id"
                    label="Email"
                    variant="outlined"
                    size="small"
                    value={formik.values.email_id}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.email_id && Boolean(formik.errors.email_id)
                    }
                    helperText={
                      formik.touched.email_id && formik.errors.email_id
                    }
                  />
                </Grid>
                <Grid item xs={2} sm={4} md={4}>
                  <Customs.CustomTextField
                    name="user_password"
                    label="Password"
                    variant="outlined"
                    type={passwordShown ? 'text' : 'password'}
                    size="small"
                    value={formik.values.user_password}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.user_password &&
                      Boolean(formik.errors.user_password)
                    }
                    helperText={
                      formik.touched.user_password &&
                      formik.errors.user_password
                    }
                    InputProps={{
                      endAdornment: (
                        <InputAdornment position="end">
                          <IconButton
                            onMouseDown={(e) => handleMouseDownPassword(e)}
                          >
                            {passwordShown ? (
                              <VisibilityIcon onClick={togglePassword} />
                            ) : (
                              <VisibilityOff
                                onClick={togglePassword}
                                style={{ color: '#BEBFC5' }}
                              />
                            )}
                          </IconButton>
                        </InputAdornment>
                      ),
                    }}
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
                  <Customs.CustomButton
                    type="submit"
                    label="Submit"
                    variant="outlined"
                    color="primary"
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
