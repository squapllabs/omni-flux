import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import {
  createSubSubcategory,
  updateSubSubcategory,
} from '../../hooks/subSubCategory-hooks';
import {
  getUpdateValidateyup,
  getCreateValidateyup,
} from '../../helper/constants/category/subsubcategory-constants';
import SubSubCategoryService from '../../service/subSubCategory-service';
import { useGetAllSubcategoryDrop } from '../../hooks/subCategory-hooks';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import Select from '../ui/selectNew';
import Styles from '../../styles/subSubCategoryList.module.scss';
import CancelIcon from '../menu/icons/closeIcon'

//Function for Sub Sub Category
const SubSubCategoryForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'ADD'
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const { data: getAllSubCategory = [] } = useGetAllSubcategoryDrop();
  const { mutate: createNewSubSubCategory } = createSubSubcategory();
  const { mutate: updateSubSubCategoryData } = updateSubSubcategory();
  const [initialValues, setInitialValues] = useState({
    sub_sub_category_id: '',
    name: '',
    budget: '',
    sub_category_id: '',
  });

  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await SubSubCategoryService.getOneSubSubcategoryByID(
          props.subSubCategoryId
        );
        setInitialValues({
          sub_sub_category_id: data?.data?.sub_sub_category_id,
          name: data?.data?.name,
          budget: data?.data?.budget,
          sub_category_id: data?.data?.sub_category_id,
        });
      };
      fetchOne();
    }
  }, []);
  //Function for adding and updating the sub sub category
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          sub_category_id: values.sub_category_id,
        };
        createNewSubSubCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpenPopup(false);
              props.setReload(true);
              props.setMessage('Sub Sub Category created');
              props.setOpenSnack(true);
            }
          },
        });
      } else {
        const Object: any = {
          sub_sub_category_id: values.sub_sub_category_id,
          name: values.name,
          budget: parseFloat(values.budget),
          sub_category_id: values.sub_category_id,
        };
        updateSubSubCategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              props.setOpenPopup(false);
              props.setReload(true);
              props.setMessage('Sub Sub Category edited');
              props.setOpenSnack(true);
            }
          },
        });
      }
    },
  });
  //Function for closing the popup
  const handleClose = () => {
    props.setOpenPopup(false);
  }

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.header}>
          <div><h4 className={Styles.titleStyle}>Edit Sub Sub Category</h4></div>
          <div> <CancelIcon onClick={handleClose} /></div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Select
            label="Sub Category"
            name="sub_category_id"
            onChange={formik.handleChange}
            mandatory={true}
            value={formik.values.sub_category_id}
            defaultLabel="Select from options"
            error={
              formik.touched.sub_category_id && formik.errors.sub_category_id
            }
            width="100%"
          >
            {getAllSubCategory.map((option: any) => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </Select>
        </div>
        <div className={Styles.field}>
          <Input
            name="name"
            label="Sub sub Category Name"
            placeholder="Enter sub sub category name"
            value={formik.values.name}
            mandatory={true}
            onChange={formik.handleChange}
            error={formik.touched.name && formik.errors.name}
          />
        </div>
        <div className={Styles.field}>
          <Input
            name="budget"
            label="Budget"
            placeholder="Enter budget"
            mandatory={true}
            value={formik.values.budget}
            onChange={formik.handleChange}
            error={formik.touched.budget && formik.errors.budget}
          />
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.formButton}>
          <div>
            <Button className={Styles.cancelButton} shape="rectangle" justify="center" onClick={handleClose}>
              Cancel
            </Button>
          </div>
          <div>
            <Button color="primary" shape="rectangle" justify="center">
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default SubSubCategoryForm;
