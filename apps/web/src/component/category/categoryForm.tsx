import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import Customs from '../ui/custom';
import { Grid, InputLabel, TextareaAutosize } from '@mui/material';
import { createCategory, updateCategory } from '../../hooks/category-hooks';
import { getClientValidateyup } from '../../helper/constants/category/category-constants';
import CategoryService from '../../service/category-service';
import * as Yup from 'yup';
const validationSchema = getClientValidateyup(Yup);
const uomForm: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    category_id: '',
    name: '',
    budget: '',
    project_id: '',
  });
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await CategoryService.getOneCategoryByID(props.uomId);
        setInitialValues({
          category_id: data?.data?.category_id,
          name: data?.data?.name,
          budget: data?.data?.budget,
          project_id: data?.data?.project_id,
        });
      };

      fetchOne();
    }
  }, []);
  const { mutate: createNewCategory, isLoading } = createCategory();
  const { mutate: updateCategoryData } = updateCategory();
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          project_id: 1,
        };
        createNewCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Category created');
              props.setOpenSnack(true);
            } else {
            }
          },
        });
      } else {
        const Object: any = {
          category_id: values.category_id,
          name: values.name,
          budget: values.budget,
        };
        updateCategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Category edited');
              props.setOpenSnack(true);
            } else {
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
              fullWidth
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

export default uomForm;
