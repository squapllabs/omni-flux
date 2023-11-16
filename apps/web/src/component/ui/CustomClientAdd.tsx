import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles1 from '../../styles/userList.module.scss';
import { useInstantcreateClient } from '../../hooks/client-hooks';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import { getClientValidateyup } from '../../helper/constants/client-constants';
import CustomSnackBar from '../ui/customSnackBar';
import Styles from '../../styles/newStyles/uomForm.module.scss';

const CustomClientAdd = (props: { isVissible: any; onAction: any }) => {
  const { isVissible, onAction } = props;
  const validationSchemaClient = getClientValidateyup(Yup);
  const { mutate: createNewClient } = useInstantcreateClient();
  const [clientinitialValues, setclientInitialValues] = useState({
    name: '',
    contact_details: '',
  });
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const formikOne = useFormik({
    initialValues: clientinitialValues,
    validationSchema: validationSchemaClient,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        name: values.name,
        contact_details: values.contact_details,
      };
      createNewClient(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Client created');
            setOpenSnack(true);
            handleCloseForm();
            resetForm();
          }
        },
      });
    },
  });

  const handleCloseForm = () => {
    onAction(false);
    formikOne.resetForm();
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div>
      <div>
        {isVissible && (
          <div className={Styles.formFields}>
            <form onSubmit={formikOne.handleSubmit}>
              <div style={{ width: '60%' }}>
                <div>
                  <Input
                    label="Name"
                    placeholder="Enter client name"
                    name="name"
                    mandatory={true}
                    value={formikOne.values.name}
                    onChange={formikOne.handleChange}
                    error={formikOne.touched.name && formikOne.errors.name}
                    // width="100%"
                  />
                </div>
                <div>
                  <Input
                    label="Contact Number"
                    placeholder="Enter client contact number"
                    name="contact_details"
                    value={formikOne.values.contact_details}
                    onChange={formikOne.handleChange}
                    error={
                      formikOne.touched.contact_details &&
                      formikOne.errors.contact_details
                    }
                    mandatory={true}
                    // width="100%"
                  />
                </div>
              </div>
              <div className={Styles.bottom_container_client}>
                <div className={Styles.footer1}>
                  <div>
                    <div className={Styles.dividerStyle}></div>
                    <div className={Styles.button}>
                      <Button
                        className={Styles.cancelButton}
                        shape="rectangle"
                        justify="center"
                        size="small"
                        onClick={handleCloseForm}
                      >
                        Cancel
                      </Button>
                      <Button
                        color="primary"
                        shape="rectangle"
                        justify="center"
                        size="small"
                        type="submit"
                      >
                        Submit
                      </Button>
                    </div>
                  </div>
                </div>
              </div>
              {/* <div className={Styles.dividerStyle}></div> */}
              {/* <div className={Styles.formButton}>
                  <div>
                    <Button
                      className={Styles.cancelButton}
                      shape="rectangle"
                      justify="center"
                      size="small"
                      onClick={handleCloseForm}
                    >
                      Cancel
                    </Button>
                  </div>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      type="submit"
                    >
                      Submit
                    </Button>
                  </div>
                </div> */}
            </form>
          </div>
        )}
      </div>
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default CustomClientAdd;
