import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { Grid, InputLabel, MenuItem, Select } from '@mui/material';
import {
  createSubSubcategory,
  updateSubSubcategory,
} from '../../hooks/subSubCategory-hooks';
import {
  getUpdateValidateyup,
  getCreateValidateyup,
} from '../../helper/constants/category/subsubcategory-constants';
import SubSubCategoryService from '../../service/subSubCategory-service';
import { useGetAllSubcategory } from '../../hooks/subCategory-hooks';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';

const SubSubCategoryForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'ADD'
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const { data: getAllSubCategory } = useGetAllSubcategory();
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
  const { mutate: createNewSubSubCategory } = createSubSubcategory();
  const { mutate: updateSubSubCategoryData } = updateSubSubcategory();
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
            if (data?.success) {
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

  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <Grid
          container
          spacing={{ xs: 2, md: 3 }}
          columns={{ xs: 4, sm: 8, md: 12 }}
        >
          <Grid item xs={2} sm={4} md={6}>
            <InputLabel id="role_id-label">Sub Category</InputLabel>
            <Select
              labelId="role_id-label"
              name="sub_category_id"
              size="small"
              sx={{ width: '300px' }}
              value={formik.values.sub_category_id}
              onChange={formik.handleChange}
              disabled={props.mode === 'EDIT' ? true : false}
            >
              {getAllSubCategory &&
                getAllSubCategory.map((option: any) => (
                  <MenuItem
                    key={option.sub_category_id}
                    value={option.sub_category_id}
                  >
                    {option.name}
                  </MenuItem>
                ))}
            </Select>
            {formik.errors.sub_category_id &&
              formik.touched.sub_category_id && (
                <div style={{ color: 'red' }}>
                  {formik.errors.sub_category_id}
                </div>
              )}
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Input
              name="name"
              label="Sub sub Category Name"
              placeholder="Enter sub sub category name"
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

export default SubSubCategoryForm;
