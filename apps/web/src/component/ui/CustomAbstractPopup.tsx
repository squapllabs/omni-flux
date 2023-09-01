import React, { useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/customaddabstract.module.scss';
import { createAbstract } from '../../hooks/abstract-hooks';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import { getAbstractValidateyup } from '../../helper/constants/abstract-constants';
import CustomSnackBar from '../ui/customSnackBar';
import TextArea from '../ui/CustomTextArea';

const CustomClientAdd = (props: { isVissible: any; onAction: any }) => {
  const { isVissible, onAction } = props;
  const validationSchemaAbstract = getAbstractValidateyup(Yup);
  const { mutate: createNewAbstract } = createAbstract();
  const [clientinitialValues, setclientInitialValues] = useState({
    name: '',
    description: '',
  });
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const formik = useFormik({
    initialValues: clientinitialValues,
    validationSchema: validationSchemaAbstract,
    enableReinitialize: true,
    onSubmit: (values,{ resetForm }) => {
      const Object: any = {
        name: values.name,
        description: values.description,
      };
      console.log(Object);
      resetForm();
      //   createNewAbstract(Object, {
      //     onSuccess: (data, variables, context) => {
      //       if (data?.success) {
      //         setMessage('Bom created');
      //         setOpenSnack(true);
      //         handleCloseForm();
      //       }
      //     },
      //   });
    },
  });

  const handleCloseForm = () => {
    onAction(false);
    formik.resetForm();
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div>
      <div>
        {isVissible && (
          <CustomPopup className="sample">
            <div className={Styles.popupContent}>
              <form onSubmit={formik.handleSubmit}>
                <div className={Styles.header}>
                  <div>
                    <h4>Add Abstract</h4>
                  </div>
                  <div>
                    <CloseIcon onClick={handleCloseForm} />
                  </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                <div className={Styles.inputFields}>
                  <div>
                    <Input
                      label="Name"
                      placeholder="Enter abstract name"
                      name="name"
                      mandatory={true}
                      value={formik.values.name}
                      onChange={formik.handleChange}
                      error={formik.touched.name && formik.errors.name}
                      width="60%"
                    />
                  </div>
                  <div>
                    <TextArea
                      name="description"
                      label="Description"
                      placeholder="Enter description"
                      value={formik.values.description}
                      onChange={formik.handleChange}
                      mandatory={true}
                      error={
                        formik.touched.description && formik.errors.description
                      }
                      rows={5}
                      maxCharacterCount={150}
                    />
                  </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                <div className={Styles.formButton}>
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
                      Save
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

