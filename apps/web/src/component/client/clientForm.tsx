import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import Customs from '../ui/custom';
import { Grid, InputLabel } from '@mui/material';
import { createClient, updateClient } from '../../hooks/client-hooks';
import { getClientValidateyup } from '../../helper/constants/client-constants';
import clientService from '../../service/client-service';
import * as Yup from 'yup';
const validationSchema = getClientValidateyup(Yup);
const ClientForm: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    name: '',
    contact_details: '',
    client_id: '',
  });
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await clientService.getOneClientByID(props.uomId);
        setInitialValues({
          client_id: data?.data?.client_id,
          name: data?.data?.name,
          contact_details: data?.data?.contact_details,
        });
      };

      fetchOne();
    }
  }, []);
  const { mutate: createNewClient, isLoading } = createClient();
  const { mutate: updateClientDetails } = updateClient();
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
        const Object: any = {
          name: values.name,
          contact_details: values.contact_details,
        };
        createNewClient(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Client created');
              props.setOpenSnack(true);
            } else {
            }
          },
        });
      } else {
        const Object: any = {
          client_id: values.client_id,
          name: values.name,
          contact_details: values.contact_details,
        };
        updateClientDetails(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Client edited');
              props.setOpenSnack(true);
            } else {
            }
          },
        });
      }
    },
  });

  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <Grid
          container
          spacing={{ xs: 2, md: 3 }}
          columns={{ xs: 4, sm: 8, md: 12 }}
        >
          <Grid item xs={2} sm={4} md={12}>
            <Customs.CustomTextField
              name="name"
              label="Name"
              variant="outlined"
              size="small"
              value={formik.values.name}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={formik.touched.name && Boolean(formik.errors.name)}
              helperText={formik.touched.name && formik.errors.name}
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Customs.CustomTextField
              name="contact_details"
              label="Contact Detail"
              variant="outlined"
              fullWidth
              value={formik.values.contact_details}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={
                formik.touched.contact_details &&
                Boolean(formik.errors.contact_details)
              }
              helperText={
                formik.touched.contact_details && formik.errors.contact_details
              }
            />
          </Grid>
          <Grid item xs={2} sm={4} md={6}>
            <Customs.CustomButton
              type="submit"
              label="Submit"
              variant="contained"
              color="primary"
            />
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default ClientForm;
