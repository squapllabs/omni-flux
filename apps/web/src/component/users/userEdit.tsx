import React, { useState, useEffect } from 'react';
import Customs from '../ui/custom';
import Styles from '../../styles/user.module.scss';
import { useFormik } from 'formik';
import { Formik, Form, Field, ErrorMessage } from 'formik';
import * as Yup from 'yup';
import { getUsercreationYupschema } from '../../helper/constants/user-constants';
import { Grid } from '@mui/material';
import { getByuserID, updateUser } from '../../hooks/user-hooks';
import { useParams } from 'react-router-dom';
const validationSchema = getUsercreationYupschema(Yup);
const UserEdit = () => {
  const routeParams = useParams();
  const { data: getOneuserData, isLoading } = getByuserID(
    Number(routeParams?.id)
  );
  const { mutate: updateUserData } = updateUser();
  console.log('getOneuserData', getOneuserData?.data);
  const [initialValues, setInitialValues] = useState({
    first_name: '',
    last_name: '',
    user_password: '',
    email_id: '',
    contact_no: '',
    user_status: '',
  });
  useEffect(() => {
    if (getOneuserData?.data) {
      setInitialValues({
        first_name: getOneuserData.data.first_name || '',
        last_name: getOneuserData.data.last_name || '',
        user_password: getOneuserData.data.user_password || '',
        email_id: getOneuserData.data.email_id || '',
        contact_no: getOneuserData.data.contact_no || '',
        user_status: getOneuserData.data.user_status || '',
      });
    }
  }, [getOneuserData]);
  const options = [
    { value: 'AC', label: 'Active' },
    { value: 'IC', label: 'In-Active' },
  ];
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      console.log(values);
      const Object: any = {
        first_name: values.first_name,
        last_name: values.last_name,
        user_password: values.user_password,
        email_id: values.email_id,
        user_status: 'AC',
        contact_no: values.contact_no,
        role_id: 1,
        user_id: Number(routeParams?.id),
      };
      updateUserData(Object, {
        onSuccess: (data, variables, context) => {
          console.log('data', data);
        },
      });
    },
  });
  if (isLoading) {
    return <div>Loading...</div>;
  }
  return (
    <>
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
                    name="email"
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
                  {/* <h3>Status</h3> */}
                  <Customs.CustomSelect
                    label="Status"
                    name="user_status"
                    size="small"
                    sx={{ width: '300px' }}
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
      </div>
    </>
  );
};

export default UserEdit;
