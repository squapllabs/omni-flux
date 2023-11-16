import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import {
  useCreateSubcategory,
  useCreateInstantSubcategory,
} from '../../hooks/subCategory-hooks';
import {
  getUpdateValidateyup,
  getCreateValidateyup,
} from '../../helper/constants/category/subcategory-constants';
import SubcategoryService from '../../service/subCategory-service';
import { useGetAllCategoryForDrop } from '../../hooks/category-hooks';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import Select from '../ui/selectNew';
import Styles from '../../styles/subCategoryList.module.scss';
import CancelIcon from '../menu/icons/closeIcon';

////Function for Sub Category
const SubCategoryForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'ADD'
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const { data: getAllCategory = [] } = useGetAllCategoryForDrop();
  const { mutate: createNewSubcategory } = useCreateSubcategory();
  const { mutate: createInstantSubcategoryData } =
    useCreateInstantSubcategory();
  const [initialValues, setInitialValues] = useState({
    sub_category_id: '',
    name: '',
    budget: '',
    category_id: '',
  });

  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await SubcategoryService.getOneSubcategoryByID(
          props.subCategoryId
        );
        setInitialValues({
          sub_category_id: data?.data?.sub_category_id,
          name: data?.data?.name,
          budget: data?.data?.budget,
          category_id: data?.data?.category_id,
        });
      };
      fetchOne();
    }
  }, []);

  //Function for Adding and Editing Sub Category
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          category_id: values.category_id,
        };
        createNewSubcategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Category created');
              props.setOpenSnack(true);
            }
          },
        });
      } else {
        const Object: any = {
          sub_category_id: values.sub_category_id,
          name: values.name,
          budget: Number(values.budget),
          category_id: Number(values.category_id),
        };
        createInstantSubcategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Category edited');
              props.setOpenSnack(true);
            }
          },
        });
      }
    },
  });

  //Function for closing the popup
  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.header}>
          <div>
            <h4 className={Styles.titleStyle}>Edit Sub Category</h4>
          </div>
          <div>
            {' '}
            <CancelIcon onClick={handleClose} />
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Select
            label="Category"
            name="category_id"
            onChange={formik.handleChange}
            value={formik.values.category_id}
            mandatory={true}
            defaultLabel="Select from options"
            width="100%"
            error={formik.touched.category_id && formik.errors.category_id}
          >
            {getAllCategory.map((option: any) => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </Select>
        </div>
        <div className={Styles.field}>
          <Input
            name="name"
            label="Sub Category Name"
            placeholder="Enter sub category name"
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
            value={formik.values.budget}
            mandatory={true}
            onChange={formik.handleChange}
            error={formik.touched.budget && formik.errors.budget}
          />
        </div>
        <div className={Styles.dividerStyle} />
        <div className={Styles.formButton}>
          <div>
            <Button
              className={Styles.cancelButton}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleClose}
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
            >
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default SubCategoryForm;
