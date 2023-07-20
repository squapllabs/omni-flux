import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import Customs from '../ui/custom';
import { Grid, InputLabel, MenuItem, Select } from '@mui/material';
import {
  createSubcategory,
  updateSubcategory,
} from '../../hooks/subCategory-hooks';
import { getSubcategoryValidateyup } from '../../helper/constants/category/subcategory-constants';
import SubcategoryService from '../../service/subCategory-service';
import { useGetAllCategory } from '../../hooks/category-hooks';
import * as Yup from 'yup';

const validationSchema = getSubcategoryValidateyup(Yup);
const UomForm: React.FC = (props: any) => {
  const { data: getAllCategory } = useGetAllCategory();
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
          budget: values.budget,
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
            <Select
              labelId="category_id-label"
              name="category_id"
              size="small"
              sx={{ width: '300px' }}
              value={formik.values.category_id}
              onChange={formik.handleChange}
            >
              {getAllCategory &&
                getAllCategory.map((option: any) => (
                  <MenuItem key={option.category_id} value={option.category_id}>
                    {option.name}
                  </MenuItem>
                ))}
            </Select>
            {formik.errors.category_id && formik.touched.category_id && (
              <div style={{ color: 'red' }}>{formik.errors.category_id}</div>
            )}
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Customs.CustomTextField
              name="name"
              label="Name"
              variant="outlined"
              size="small"
              value={formik.values.name}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={formik.touched.name && Boolean(formik.errors.name)}
              helperText={formik.touched.name && formik.errors.name}
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Customs.CustomTextField
              name="budget"
              label="Budget"
              variant="outlined"
              value={formik.values.budget}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={formik.touched.budget && Boolean(formik.errors.budget)}
              helperText={formik.touched.budget && formik.errors.budget}
            />
          </Grid>
          <Grid item xs={2} sm={4} md={6}>
            <Customs.CustomButton
              type="submit"
              label="Submit"
              variant="contained"
              color="primary"
            />
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default UomForm;
