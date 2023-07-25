import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { Grid, InputLabel, MenuItem, Select } from '@mui/material';
import { createCategory, updateCategory } from '../../hooks/category-hooks';
import {
  getCreateValidateyup,
  getUpdateValidateyup,
} from '../../helper/constants/category/category-constants';
import CategoryService from '../../service/category-service';
import * as Yup from 'yup';
import { useGetAllProject } from '../../hooks/project-hooks';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import CustomAutoDropdown from '../ui/CustomAutoDropdown';

const CategoryForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'ADD'
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const [initialValues, setInitialValues] = useState({
    category_id: '',
    name: '',
    budget: '',
    project_id: '',
  });
  const { data: getAllProjectList = [] } = useGetAllProject();

  const getFilteredProjectList = (input: string) => {
    return getAllProjectList.filter((project: any) =>
      project.label.toLowerCase().includes(input.toLowerCase())
    );
  };
  const [selectedValue, setSelectedValue] = useState('');
  // console.log('selectedValue***', selectedValue);
  // console.log('selectedValue===>', typeof selectedValue);

  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await CategoryService.getOneCategoryByID(props.categoryId);
        setInitialValues({
          category_id: data?.data?.category_id,
          name: data?.data?.name,
          budget: data?.data?.budget,
          project_id: data?.data?.project_id,
        });
        // setSelectedValue(data?.data?.project_id);
      };

      fetchOne();
    }
  }, []);
  const { mutate: createNewCategory } = createCategory();
  const { mutate: updateCategoryData } = updateCategory();
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      // console.log('values', values);
      if (props.mode === 'ADD') {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          project_id: values.project_id,
        };
        createNewCategory(Object, {
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
          category_id: values.category_id,
          name: values.name,
          budget: Number(values.budget),
          project: values.project_id,
        };
        updateCategoryData(Object, {
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
          {/* <Grid item xs={2} sm={4} md={6}> */}
          {/* <InputLabel id="project_id-label">Project</InputLabel> */}
          {/* <CustomAutoDropdown
              value={selectedValue}
              onChange={setSelectedValue}
              getOptionsFromAPI={getFilteredProjectList}
              defaultLabel="Select an option"
              width="300px"
              maxHeight="300px"
            /> */}
          {/* </Grid> */}
          <Grid item xs={2} sm={4} md={6}>
            <InputLabel id="project_id-label">Project</InputLabel>
            <Select
              labelId="project_id-label"
              name="project_id"
              size="small"
              sx={{ width: '300px' }}
              value={formik.values.project_id}
              onChange={formik.handleChange}
              disabled={props.mode === 'EDIT' ? true : false}
            >
              {getAllProjectList &&
                getAllProjectList.map((option: any) => (
                  <MenuItem key={option.project_id} value={option.project_id}>
                    {option.project_name}
                  </MenuItem>
                ))}
            </Select>
            {formik.errors.project_id && formik.touched.project_id && (
              <div style={{ color: 'red' }}>{formik.errors.project_id}</div>
            )}
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Input
              name="name"
              label="Category Name"
              placeholder="Enter category name"
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

export default CategoryForm;
