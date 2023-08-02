import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
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
import Select from '../ui/selectNew';
import { formatBudgetValue } from '../../helper/common-function';
import Styles from '../../styles/categoryList.module.scss';
import { environment } from '../../environment/environment'; 

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
  const [appendedValue, setAppendedValue] = useState();

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
        const budgetData = formatBudgetValue(Number(data?.data?.budget))
        setAppendedValue(budgetData)
      };

      fetchOne();
    }
  }, [props.mode, props.categoryId]);
  const { mutate: createNewCategory } = createCategory();
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
          project_id: Number(values.project_id),
        };
        updateCategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
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
  };

  const handleBudgetChange = (event : any) => {
    const budgetValue = event.target.value;
    const data = formatBudgetValue(Number(budgetValue))
    setAppendedValue(data);
    formik.setFieldValue('budget', budgetValue);
    formik.handleChange(event);
  };

  const inputLabelNameFromEnv = `Budget (${environment.INPUTBUDGET})`
  const outputLableNameFromEnv = `Budget (${environment.OUTPUTBUDGET})`

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div>
          <Select
            label="Project"
            name="project_id"
            onChange={formik.handleChange}
            value={formik.values.project_id}
            defaultLabel="Select from options"
            width="100%"
            error={formik.touched.project_id && formik.errors.project_id}
          >
            {getAllProjectList.map((option: any) => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </Select>
        </div>
        <div>
          <Input
            name="name"
            label="Category Name"
            placeholder="Enter category name"
            value={formik.values.name}
            onChange={formik.handleChange}
            error={formik.touched.name && formik.errors.name}
            width="100%"
          />
        </div>
        <div>
          <Input
            name="budget"
            label={inputLabelNameFromEnv}
            placeholder="Enter budget"
            value={formik.values.budget}
            // onChange={formik.handleChange}
            onChange={handleBudgetChange}
            error={formik.touched.budget && formik.errors.budget}
            width="100%"
          />
        </div>
        <div>
          <Input
            name="label_field"
            label={outputLableNameFromEnv}
            placeholder="Enter budget"
            value={appendedValue}
          />
        </div>
        <div className={Styles.formButton}>
          <div>
            <Button shape="rectangle" justify="center" size="small" onClick={handleBack}>
              Cancel
            </Button>
          </div>
          <div>
            <Button color="primary" shape="rectangle" justify="center" size="small">
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default CategoryForm;
