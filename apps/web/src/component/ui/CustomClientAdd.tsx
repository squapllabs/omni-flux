import React, { useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles1 from '../../styles/userList.module.scss';
import { instantcreateClient } from '../../hooks/client-hooks';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import { getClientValidateyup } from '../../helper/constants/client-constants';
import CustomSnackBar from '../ui/customSnackBar';

const CustomClientAdd = (props: { isVissible: any; onAction: any }) => {
  const { isVissible, onAction } = props;
  const validationSchemaClient = getClientValidateyup(Yup);
  const { mutate: createNewClient } = instantcreateClient();
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
    onSubmit: (values) => {
      const Object: any = {
        name: values.name,
        contact_details: values.contact_details,
      };
      createNewClient(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success) {
            setMessage('Client created');
            setOpenSnack(true);
            handleCloseForm();
          }
        },
      });
    },
  });

  const handleCloseForm = () => {
    onAction(false);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div>
      <div>
        {isVissible && (
          <CustomPopup
            className={
              isVissible
                ? 'popup-enter popup-enter-active'
                : 'popup-exit popup-exit-active'
            }
          >
            <div className={Styles1.popupContent}>
              <form onSubmit={formikOne.handleSubmit}>
                <div className={Styles1.header}>
                  <div className={Styles1.popupHeader}>
                    <h4>Create Client</h4>
                    <button className={Styles1.closeButton}>
                      <CloseIcon onClick={handleCloseForm} />
                    </button>
                  </div>
                </div>
                <div className={Styles1.field}>
                  <Input
                    label="Name"
                    placeholder="Enter client name"
                    name="name"
                    value={formikOne.values.name}
                    onChange={formikOne.handleChange}
                    error={formikOne.touched.name && formikOne.errors.name}
                    width="100%"
                  />
                </div>
                <div className={Styles1.field}>
                  <Input
                    label="Contact Detail"
                    placeholder="Enter client contact detail"
                    name="contact_details"
                    value={formikOne.values.contact_details}
                    onChange={formikOne.handleChange}
                    error={
                      formikOne.touched.contact_details &&
                      formikOne.errors.contact_details
                    }
                    width="100%"
                  />
                </div>
                <div className={Styles1.dividerStyle}></div>
                <div className={Styles1.formButton}>
                  <div>
                    <Button
                      className={Styles1.cancelButton}
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
                </div>
              </form>
            </div>
          </CustomPopup>
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
