import React, { useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/customaddabstract.module.scss';
import { createInstantSubcategory } from '../../hooks/subCategory-hooks';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import { getSubCategoryValidateyup } from '../../helper/constants/abstract-constants';
import CustomSnackBar from '../ui/customSnackBar';
import TextArea from '../ui/CustomTextArea';
import DatePicker from './CustomDatePicker';

const CustomSubCategoryAdd = (props: {
  isVissible: any;
  onAction: any;
  selectedCategoryId: any;
  selectedProject: any;
}) => {
  const { isVissible, onAction, selectedCategoryId, selectedProject } = props;
  console.log('!!!!!!!!!!', selectedProject);

  const validationSchemaSubCategory = getSubCategoryValidateyup(Yup);
  const { mutate: createNewSubCategory } = createInstantSubcategory();
  const [clientinitialValues, setclientInitialValues] = useState({
    name: '',
    description: '',
    project_id: '',
    category_id: '',
    start_date: '',
    end_date: '',
  });
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const formik = useFormik({
    initialValues: clientinitialValues,
    validationSchema: validationSchemaSubCategory,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        name: values.name,
        description: values.description,
        project_id: selectedProject,
        budget: 0,
        category_id: selectedCategoryId,
        start_date: values.start_date,
        end_date: values.end_date,
      };
      console.log('sub category added form ', Object);
      createNewSubCategory(Object, {
        onSuccess: (data, variables, context) => {
          console.log('samlpe data==>', data);
          if (data?.status === true) {
            setMessage('Sub Category created');
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
                    <h4>Add Tasks</h4>
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
                      placeholder="Enter task name"
                      name="name"
                      mandatory={true}
                      value={formik.values.name}
                      onChange={formik.handleChange}
                      error={formik.touched.name && formik.errors.name}
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
                  <div className={Styles.dateField}>
                    <DatePicker
                      label="Start Date"
                      name="start_date"
                      value={formik.values.start_date}
                      onChange={formik.handleChange}
                      InputProps={{
                        inputProps: {
                          min: '1930-01-01',
                          max: `${new Date().toISOString().slice(0, 10)}`,
                        },
                      }}
                      error={
                        formik.touched.start_date && formik.errors.start_date
                      }
                    />
                    <DatePicker
                      label="End Date"
                      name="end_date"
                      value={formik.values.end_date}
                      onChange={formik.handleChange}
                      error={formik.touched.end_date && formik.errors.end_date}
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

export default CustomSubCategoryAdd;
