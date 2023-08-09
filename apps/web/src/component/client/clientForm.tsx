import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { Grid } from '@mui/material';
import { createClient, updateClient } from '../../hooks/client-hooks';
import { getClientValidateyup } from '../../helper/constants/client-constants';
import clientService from '../../service/client-service';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../menu/button';
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
        const data = await clientService.getOneClientByID(props.clientId);
        setInitialValues({
          client_id: data?.data?.client_id,
          name: data?.data?.name,
          contact_details: data?.data?.contact_details,
        });
      };

      fetchOne();
    }
  }, []);
  const { mutate: createNewClient } = createClient();
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
            <Input
              label="Name"
              placeholder="Enter client name"
              name="name"
              value={formik.values.name}
              onChange={formik.handleChange}
              error={formik.touched.name && formik.errors.name}
              width="100%"
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Input
              label="Contact Detail"
              placeholder="Enter client contact detail"
              name="contact_details"
              value={formik.values.contact_details}
              onChange={formik.handleChange}
              error={
                formik.touched.contact_details && formik.errors.contact_details
              }
              width="100%"
            />
          </Grid>
          <Grid item xs={2} sm={4} md={6}>
            <Button
              text="Submit"
              backgroundColor="#7F56D9"
              fontSize={14}
              fontWeight={500}
              width={125}
            />
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default ClientForm;
