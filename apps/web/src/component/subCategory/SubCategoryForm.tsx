import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import {
  createSubcategory,
  updateSubcategory,
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

const SubCategoryForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'ADD'
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const { data: getAllCategory = [] } = useGetAllCategoryForDrop();
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
  const { mutate: createNewSubcategory } = createSubcategory();
  const { mutate: updateSubcategoryData } = updateSubcategory();


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
        updateSubcategoryData(Object, {
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

  const handleBack = () => {
    props.setOpen(false);
  }

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div>
        <Select
            label="Category"
            name="category_id"
            onChange={formik.handleChange}
            value={formik.values.category_id}
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
        <div>
          <Input
            name="name"
            label="Sub Category Name"
            placeholder="Enter sub category name"
            value={formik.values.name}
            onChange={formik.handleChange}
            error={formik.touched.name && formik.errors.name}
          />
        </div>
        <div>
          <Input
            name="budget"
            label="Budget"
            placeholder="Enter budget"
            value={formik.values.budget}
            onChange={formik.handleChange}
            error={formik.touched.budget && formik.errors.budget}
          />
        </div>
        <div className={Styles.formButton}>
          <div>
            <Button shape="rectangle" justify="center"  size="small" onClick={handleBack}>
              Cancel
            </Button>
          </div>
          <div>
            <Button color="primary" shape="rectangle" justify="center"  size="small">
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default SubCategoryForm;
