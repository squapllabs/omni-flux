import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { createClient, updateClient } from '../../hooks/client-hooks';
import { getClientValidateyup } from '../../helper/constants/client-constants';
import clientService from '../../service/client-service';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/userList.module.scss';
import CancelIcon from '../menu/icons/closeIcon';

//Function for client form
const ClientForm: React.FC = (props: any) => {
  const { mutate: createNewClient } = createClient();
  const { mutate: updateClientDetails } = updateClient();
  const validationSchema = getClientValidateyup(Yup);
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
  }, );
  //Function for creating and updating client data
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
            if (data?.message === 'success') {
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
            if (data?.message === 'success') {
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
  //Function for closing
  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.header}>
          <div><h4 className={Styles.titleStyle}>Edit Client</h4></div>
          <div> <CancelIcon onClick={handleClose} /></div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Input
            label="Name"
            placeholder="Enter client name"
            name="name"
            value={formik.values.name}
            mandatory={true}
            onChange={formik.handleChange}
            error={formik.touched.name && formik.errors.name}
            width="100%"
          />
        </div>
        <div className={Styles.field}>
          <Input
            label="Contact Detail"
            placeholder="Enter client contact detail"
            name="contact_details"
            mandatory={true}
            value={formik.values.contact_details}
            onChange={formik.handleChange}
            error={formik.touched.contact_details && formik.errors.contact_details}
            width="100%"
          />
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.formButton}>
          <div>
            <Button className={Styles.cancelButton} shape="rectangle" justify="center" size="small" onClick={handleClose}>
              Cancel
            </Button>
          </div>
          <div>
            <Button color="primary" shape="rectangle" justify="center" size="small">
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default ClientForm;
