import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/customaddabstract.module.scss';
// import { createAbstract } from '../../hooks/abstract-hooks';
import {
  createInstantCategory,
  updateCategory,
  useGetMasterAbstractStatusParentType
} from '../../hooks/category-hooks';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import { getAbstractValidateyup } from '../../helper/constants/abstract-constants';
import CustomSnackBar from '../ui/customSnackBar';
import TextArea from '../ui/CustomTextArea';
import DatePicker from './CustomDatePicker';
import CategoryService from '../../service/category-service';
import { format } from 'date-fns';
import Select from '../ui/selectNew';


const CustomAbstractAdd = (props: {
  isVissible: any;
  onAction: any;
  selectedProject: any;
  setReload: any;
  mode: any;
  categoryId: any;
  selectedBomConfig: any;
  setMode:any;
}) => {
  const {
    isVissible,
    onAction,
    selectedProject,
    setReload,
    mode,
    categoryId,
    selectedBomConfig,
    setMode,
  } = props;
  console.log("props.mode---->",mode);
  
  const validationSchemaAbstract = getAbstractValidateyup(Yup);
  const { mutate: createNewAbstract } = createInstantCategory();
  const { mutate: updateCategoryData } = updateCategory();
  const { data: getAllAbstractStatusDatadrop = [] } =
  useGetMasterAbstractStatusParentType();
  
  const [clientinitialValues, setclientInitialValues] = useState({
    name: '',
    description: '',
    project_id: '',
    start_date: '',
    end_date: '',
    category_id: '',
    selectedBomConfig: '',
    progress_status:''
  });

  const dateFormat = (value: any) => {
    if (value !== null) {
      const currentDate = new Date(value);
      const formattedDate = format(currentDate, 'yyyy-MM-dd');
      return formattedDate;
    }
  };

  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  useEffect(() => {
    if (mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await CategoryService.getOneCategoryByID(props.categoryId);
        setclientInitialValues({
          name: data?.data?.name,
          project_id: data?.data?.project_id,
          description: data?.data?.description,
          start_date: dateFormat(data?.data?.start_date),
          end_date: dateFormat(data?.data?.end_date),
          category_id: data?.data?.category_id,
          progress_status:data?.data?.progress_status
        });
        // console.log('dataaaa', data);
      };
      fetchOne();
    }
  }, [props.mode, props.categoryId]);

  const formik = useFormik({
    initialValues: clientinitialValues,
    validationSchema: validationSchemaAbstract,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (mode === 'EDIT') {
        const Object: any = {
          name: values.name,
          description: values.description,
          project_id: selectedProject,
          budget: 0,
          start_date: values.start_date,
          end_date: values.end_date,
          category_id: values.category_id,
          bom_configuration_id: selectedBomConfig,
          progress_status:values.progress_status
        };
        updateCategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setMessage('Abstract edited');
              setOpenSnack(true);
              resetForm();
              setclientInitialValues({})
              setReload(true);
              handleCloseForm();
            }
          },
        });
      } else {
        const Object: any = {
          name: values.name,
          description: values.description,
          project_id: selectedProject,
          budget: 0,
          start_date: values.start_date,
          end_date: values.end_date,
          bom_configuration_id: selectedBomConfig,
          progress_status: 'Inprogress',
        };
        createNewAbstract(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setMessage('Abstract created');
              setOpenSnack(true);
              resetForm();
              setReload(true);
              handleCloseForm();
            }
          },
        });
      }
    },
  });

  const handleCloseForm = () => {
    formik.resetForm();
    setclientInitialValues({})
    onAction(false);
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
                    <h4> {mode === 'EDIT' ? 'Edit Abstract' : 'Add Abstract'}</h4>
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
                    />
                  </div>
                  {mode === 'EDIT' ? (
                    <div>
                      <Select
                        label="Status"
                        name="progress_status"
                        mandatory={true}
                        placeholder="Select the Status"
                        onChange={formik.handleChange}
                        value={formik.values.progress_status}
                        defaultLabel="Select from options"
                      >
                        {getAllAbstractStatusDatadrop.map((option: any) => (
                          <option key={option.value} value={option.value}>
                            {option.label}
                          </option>
                        ))}
                      </Select>
                    </div>
                  ) : null}
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
                      rows={10}
                      maxCharacterCount={1000}
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

export default CustomAbstractAdd;
