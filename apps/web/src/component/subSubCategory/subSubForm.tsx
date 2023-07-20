import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import Customs from '../ui/custom';
import { Grid } from '@mui/material';
import {
  createSubSubcategory,
  updateSubSubcategory,
} from '../../hooks/subSubCategory-hooks';
import { getClientValidateyup } from '../../helper/constants/category/category-constants';
import SubSubCategoryService from '../../service/subSubCategory-service';
import * as Yup from 'yup';

const validationSchema = getClientValidateyup(Yup);
const UomForm: React.FC = (props: any) => {
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
          props.sub_sub_category_id
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
          sub_category_id: 1,
        };
        createNewSubSubCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
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
          budget: values.budget,
        };
        updateSubSubCategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
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

export default UomForm;
