import React, { useState, useEffect } from 'react';
import Customs from '../ui/custom';
import Styles from '../../styles/user.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getUsereditYupschema } from '../../helper/constants/user-constants';
import { Grid, InputLabel, MenuItem, Select } from '@mui/material';
import { getByuserID, updateUser } from '../../hooks/user-hooks';
import { useGetAllRoles } from '../../hooks/userRole-hooks';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router';
import MySnackbar from '../ui/MySnackbar';
import Input from '../ui/Input';
import Button from '../menu/button';

const validationSchema = getUsereditYupschema(Yup);
const UserEdit = () => {
  const navigate = useNavigate();
  const routeParams = useParams();
  const { data: getOneuserData, isLoading } = getByuserID(
    Number(routeParams?.id)
  );

  const { mutate: updateUserData } = updateUser();
  const { data: getAllRoles } = useGetAllRoles();
  const [OpenSnackbar, setOpenSnakBar] = useState(false);
  const [message, setMessage] = useState('');
  const handleSnackBarClose = () => {
    setOpenSnakBar(false);
  };
  const [initialValues, setInitialValues] = useState({
    first_name: '',
    last_name: '',
    email_id: '',
    contact_no: '',
    user_status: '',
    role_id: '',
    department:''
  });
  useEffect(() => {
    if (getOneuserData) {
      setInitialValues({
        first_name: getOneuserData?.userData?.first_name || '',
        last_name: getOneuserData?.userData?.last_name || '',
        email_id: getOneuserData?.userData?.email_id || '',
        contact_no: getOneuserData?.userData?.contact_no || '',
        user_status: getOneuserData?.userData?.user_status || '',
        role_id: getOneuserData?.roleId || '',
        department: getOneuserData?.userData?.department || '',
      });
    }
  }, [getOneuserData]);
  const options = [
    { value: 'AC', label: 'Active' },
    { value: 'IN', label: 'In-Active' },
  ];
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      const Object: any = {
        first_name: values.first_name,
        last_name: values.last_name,
        email_id: values.email_id,
        user_password: getOneuserData?.user_password,
        user_status: values.user_status,
        contact_no: values.contact_no,
        role_id: values.role_id,
        user_id: Number(routeParams?.id),
        department:values.department
      };
      updateUserData(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success === true) {
            setOpenSnakBar(true);
            setMessage('User Data Has updated Successfully');
            setInterval(() => {
              navigate('/userList');
            }, 3000);
          }
        },
      });
    },
  });
  if (isLoading) {
    return <div>Loading...</div>;
  }
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
                  <h2>USER EDIT</h2>
                </Grid>
                <Grid item xs={2} sm={4} md={4}>
                <Input
                  label="First Name"
                  placeholder="Enter your First name"
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
                  placeholder="Enter your Last name"
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
                  placeholder="Enter your Mobile Number"
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
                  placeholder="Enter your Email"
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
                <Grid item xs={2} sm={4} md={4}>
                  <InputLabel id="status_id-label">Status</InputLabel>
                  <Customs.CustomSelect
                    labelID="status_id-label"
                    name="user_status"
                    size="small"
                    sx={{ width: '325px' }}
                    options={options}
                    value={formik.values.user_status}
                    onChange={formik.handleChange}
                  />
                  {formik.errors.user_status && formik.touched.user_status && (
                    <div style={{ color: 'red' }}>
                      {formik.errors.user_status}
                    </div>
                  )}
                </Grid>
                <Grid item xs={2} sm={4} md={4}>
                  <InputLabel id="role_id-label">Role</InputLabel>
                  <Select
                    labelId="role_id-label"
                    name="role_id"
                    size="small"
                    sx={{ width: '320px' }}
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
          severity={'success'}
          autoHideDuration={1000}
        />
      </div>
  );
};

export default UserEdit;
