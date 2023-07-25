import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { Grid, InputLabel } from '@mui/material';
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
import SelectDrop from '../ui/Select';

const SubCategoryForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'ADD'
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const { data: getAllCategory } = useGetAllCategoryForDrop();
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

  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedCategoryId = event.target.value;
    formik.setFieldValue('category_id', Number(selectedCategoryId));
  }

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
          category_id: values.category_id,
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

  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <Grid
          container
          spacing={{ xs: 2, md: 3 }}
          columns={{ xs: 4, sm: 8, md: 12 }}
        >
          <Grid item xs={2} sm={4} md={6}>
            <InputLabel id="category_id-label">Category</InputLabel>
            <SelectDrop
                  options={getAllCategory}
                  onChange={handleDropdownChange}
                  value={formik.values.category_id}
                  defaultLabel="Select from options"
                  width="100%"
                />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Input
              name="name"
              label="Sub Category Name"
              placeholder="Enter sub category name"
              value={formik.values.name}
              onChange={formik.handleChange}
              error={formik.touched.name && formik.errors.name}
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Input
              name="budget"
              label="Budget"
              placeholder="Enter budget"
              value={formik.values.budget}
              onChange={formik.handleChange}
              error={formik.touched.budget && formik.errors.budget}
            />
          </Grid>
          <Grid item xs={2} sm={4} md={6}>
            <Button color="primary" shape="rectangle" justify="center">
              Submit
            </Button>
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default SubCategoryForm;
