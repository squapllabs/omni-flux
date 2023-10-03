import React, { useEffect, useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import {
  createInstantSubcategory,
  updateSubcategory,
} from '../../../hooks/subCategory-hooks';
import { getSubCategoryValidateyup } from '../../../helper/constants/abstract-constants';
import TextArea from '../../ui/CustomTextArea';
import DatePicker from '../../ui/CustomDatePicker';
import SubcategoryService from '../../../service/subCategory-service';
import { format } from 'date-fns';
import Styles from '../../../styles/newStyles/project_abstractAdd.module.scss';
import DeliveryTruckIcon from '../../menu/icons/deliveryTruckicon';

const ProjectTaskAdd: React.FC = (props: any) => {
  const validationSchemaSubCategory = getSubCategoryValidateyup(Yup);
  const { mutate: createNewSubCategory } = createInstantSubcategory();
  const { mutate: updateSubcategoryData } = updateSubcategory();
  const [initialValues, setInitialValues] = useState({
    name: '',
    description: '',
    project_id: '',
    category_id: '',
    start_date: '',
    end_date: '',
    sub_category_id: '',
    budget: '',
  });
  const dateFormat = (value: any) => {
    if (value !== null) {
      const currentDate = new Date(value);
      const formattedDate = format(currentDate, 'yyyy-MM-dd');
      return formattedDate;
    }
  };
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await SubcategoryService.getOneSubcategoryByID(
          props.selectedSubCategory
        );
        setInitialValues({
          name: data?.data?.name,
          description: data?.data?.description,
          start_date: dateFormat(data?.data?.start_date),
          end_date: dateFormat(data?.data?.end_date),
          category_id: data?.data?.category_id,
          project_id: data?.data?.project_id,
          sub_category_id: data?.data?.sub_category_id,
          bom_configuration_id: props.selectedBomConfig,
          budget: data?.data?.budget,
        });
      };
      fetchOne();
    }
  }, [props.mode, props.selectedSubCategory]);

  const formik = useFormik({
    initialValues: initialValues,
    validationSchema: validationSchemaSubCategory,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (props.mode === 'EDIT') {
        const Object: any = {
          name: values.name,
          description: values.description,
          project_id: props.selectedProject,
          budget: initialValues.budget,
          category_id: props.selectedCategoryId,
          start_date: values.start_date,
          end_date: values.end_date,
          sub_category_id: values.sub_category_id,
        };
        console.log('abstract from', Object);
        updateSubcategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              props.setMessage('Task edited');
              props.setOpenSnack(true);
            //   props.setMode('ADD');
              props.setOpen(false);
              props.setReload(true);
            //   handleClose();
              resetForm();
            }
          },
        });
      } else {
        const Object: any = {
          name: values.name,
          description: values.description,
          project_id: props.selectedProject,
          budget: 0,
          category_id: props.selectedCategoryId,
          start_date: values.start_date,
          end_date: values.end_date,
          bom_configuration_id: props.selectedBomConfig,
        };
        console.log('sub category added form ', Object);
        createNewSubCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              props.setMessage('Task created');
              props.setOpenSnack(true);
              //   handleClose();
              props.setOpen(false);
              props.setReload(true);
              resetForm();
            }
          },
        });
      }
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.divOne}>
        <div style={{ width: '70%' }}>
          <div className={Styles.field}>
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
          <div className={Styles.field}>
            <TextArea
              name="description"
              label="Description"
              placeholder="Enter description"
              value={formik.values.description}
              onChange={formik.handleChange}
              mandatory={true}
              error={formik.touched.description && formik.errors.description}
              rows={10}
              maxCharacterCount={1000}
            />
          </div>
          <div className={Styles.field}>
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
              error={formik.touched.start_date && formik.errors.start_date}
              width="200px"
            />
          </div>
          <div className={Styles.field}>
            <DatePicker
              label="End Date"
              name="end_date"
              value={formik.values.end_date}
              onChange={formik.handleChange}
              error={formik.touched.end_date && formik.errors.end_date}
              width="200px"
            />
          </div>
        </div>
        <div className={Styles.icon}>
          <DeliveryTruckIcon />
        </div>
      </div>
      <div className={Styles.sub_sub_container_2}>
        <div className={Styles.footer}>
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
  );
};
export default ProjectTaskAdd;
