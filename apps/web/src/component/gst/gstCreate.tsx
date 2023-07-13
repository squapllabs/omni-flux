import React, { useState } from 'react';
import Customs from '../ui/custom';
import Styles from '../../styles/gstAdd.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getGstcreationYupschema } from '../../helper/constants/gst-constants';
import { Grid } from '@mui/material';
import { createGst } from '../../hooks/gst-hooks';
import { useNavigate } from 'react-router';
import MySnackbar from '../ui/MySnackbar';
const validationSchema = getGstcreationYupschema(Yup);
const UserCreate = () => {
  const navigate = useNavigate();
  const [OpenSnackbar, setOpenSnakBar] = useState(false);
  const [isWarning, setIsWarning] = useState(false);
  const [message, setMessage] = useState('');
  const handleSnackBarClose = () => {
    setOpenSnakBar(false);
  };
  const [initialValues, setInitialValues] = useState({
    rate: '',
    cgst_rate: '',
    igst_rate: '',
  });

  const { mutate: createNewGst, isLoading } = createGst();

  const formik = useFormik({
    initialValues,
    validationSchema,
    onSubmit: (values) => {
      const Object: any = {
        rate: parseFloat(values.rate),
        cgst_rate: parseFloat(values.cgst_rate),
        igst_rate: parseFloat(values.igst_rate),
      };
      createNewGst(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success) {
            setMessage('Gst created successfully');
            setOpenSnakBar(true);
            setInterval(() => {
                navigate('/gst-list');
            }, 3000);
          } else {
            setIsWarning(true);
            setMessage('Err in post');
            setOpenSnakBar(true);
          }
        },
      });
    },
  });
  return (
    <>
      <div className={Styles.container}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.fields}>
                <div>
                  <h2>Gst Creation</h2>
                </div>
                <Grid item xs={2} sm={4} md={4}>
                  <Customs.CustomTextField
                    name="rate"
                    label="Gst Rate"
                    variant="outlined"
                    size="small"
                    value={formik.values.rate}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.rate &&
                      Boolean(formik.errors.rate)
                    }
                    helperText={
                      formik.touched.rate && formik.errors.rate
                    }
                  />
                </Grid>

                <Grid item xs={2} sm={4} md={4}>
                  <Customs.CustomTextField
                    name="cgst_rate"
                    label="Cgst Rate"
                    variant="outlined"
                    size="small"
                    value={formik.values.cgst_rate}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.cgst_rate &&
                      Boolean(formik.errors.cgst_rate)
                    }
                    helperText={
                      formik.touched.cgst_rate && formik.errors.cgst_rate
                    }
                  />
                </Grid>

                <Grid item xs={2} sm={4} md={4} >
                  <Customs.CustomTextField
                    name="igst_rate"
                    label="Igst Rate"
                    variant="outlined"
                    size="small"
                    value={formik.values.igst_rate}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    error={
                      formik.touched.igst_rate &&
                      Boolean(formik.errors.igst_rate)
                    }
                    helperText={
                      formik.touched.igst_rate && formik.errors.igst_rate
                    }
                  />
                </Grid>

                <Grid item xs={2} sm={4} md={12}>
                  <Customs.CustomButton
                    type="submit"
                    label="Submit"
                    variant="outlined"
                    color="primary"
                  />
                </Grid>
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
    </>
  );
};

export default UserCreate;
