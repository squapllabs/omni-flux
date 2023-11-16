import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { useCreateClient, useUpdateClient } from '../../hooks/client-hooks';
import { getClientValidateyup, getUpdateClientValidateyup } from '../../helper/constants/client-constants';
import clientService from '../../service/client-service';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/newStyles/uomForm.module.scss';

//Function for client form
const ClientForm: React.FC = (props: any) => {
  const { mutate: createNewClient } = useCreateClient();
  const { mutate: updateClientDetails } = useUpdateClient();
  const validationSchema = props.mode === 'ADD' ? getClientValidateyup(Yup) : getUpdateClientValidateyup(Yup);
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
  });
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
    <div>
      <div className={Styles.sub_container}>
        <div className={Styles.formFields}>
          <div style={{ width: '60%' }}>
            <div>
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
            <div>
              <Input
                label="Contact Number"
                placeholder="Enter client contact Number"
                name="contact_details"
                mandatory={true}
                value={formik.values.contact_details}
                onChange={formik.handleChange}
                error={
                  formik.touched.contact_details &&
                  formik.errors.contact_details
                }
                width="100%"
              />
            </div>
          </div>
        </div>
        <div className={Styles.bottom_container_client}>
          <div className={Styles.footer1}>
            <div>
              <div className={Styles.dividerStyle}></div>
              <div className={Styles.button}>
                <Button
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={handleClose}
                  className={Styles.cancelButton}
                >
                  Cancel
                </Button>
                <Button
                  shape="rectangle"
                  color="primary"
                  justify="center"
                  size="small"
                  type="submit"
                  onClick={formik.handleSubmit}
                >
                  Save
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ClientForm;
