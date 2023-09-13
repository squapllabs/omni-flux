import React, { useState, useEffect } from 'react';
import { createCategory, updateCategory } from '../../hooks/category-hooks';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import { formatBudgetValue } from '../../helper/common-function';
import { environment } from '../../environment/environment';
import { useNavigate } from 'react-router-dom';
import Input from '../ui/Input';
import TextArea from '../ui/CustomTextArea';
import Button from '../ui/Button';
import { useFormik } from 'formik';
import { useParams } from 'react-router-dom';
import * as Yup from 'yup';
import Styles from '../../styles/categoryForm.module.scss';
import {
  getCreateValidateyup,
  getUpdateValidateyup,
} from '../../helper/constants/category/category-constants';
import CustomSnackBar from '../ui/customSnackBar';
import CategoryService from '../../service/category-service';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';

const CategotyAdd: React.FC = (props: any) => {
  const { mutate: createNewCategory } = createCategory();
  const { mutate: updateOneCategory } = updateCategory();
  const { data: getAllProjectList = [] } = useGetAllProjectDrop();

  const routeParams = useParams();
  const validationSchema =
    routeParams?.id === undefined
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const navigate = useNavigate();
  const [initialValues, setInitialValues] = useState({
    category_id: '',
    name: '',
    budget: '',
    description: '',
    project_id: '',
  });

  const [disable, setDisable] = useState(
    routeParams?.id !== undefined ? true : false
  );
  const [appendedValue, setAppendedValue] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const inputLabelNameFromEnv = `Budget (${environment.INPUTBUDGET})`;
  const outputLableNameFromEnv = `Budget (${environment.OUTPUTBUDGET})`;

  useEffect(() => {
    if (Number(routeParams?.id)) {
      const fetchOne = async () => {
        const data = await CategoryService.getOneCategoryByID(
          Number(routeParams?.id)
        );
        setInitialValues({
          category_id: data?.data?.category_id,
          name: data?.data?.name,
          budget: data?.data?.budget,
          description: data?.data?.description,
          project_id: data?.data?.project_id,
        });
        const budgetData = formatBudgetValue(Number(data?.data?.budget));
        setAppendedValue(budgetData);
      };
      fetchOne();
    }
  }, [routeParams?.id]);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (Number(routeParams?.id)) {
        const Object: any = {
          category_id: values.category_id,
          description: values.description,
          budget: Number(values.budget),
          project_id: Number(values.project_id),
        };
        updateOneCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Category Edited');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      } else {
        const Object: any = {
          name: values.name,
          description: values.description,
          budget: Number(values.budget),
          project_id: Number(values.project_id),
        };
        createNewCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Category created');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      }
    },
  });

  const handleBudgetChange = (event: any) => {
    const budgetValue = event.target.value;
    const data = formatBudgetValue(Number(budgetValue));
    setAppendedValue(data);
    formik.setFieldValue('budget', budgetValue);
    formik.handleChange(event);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div className={Styles.formContainer}>
      <div className={Styles.box}>
        <div>
          <h3>Category Add/Edit</h3>
        </div>
      </div>
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.form}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.formFields}>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="name"
                  label="Category Name"
                  placeholder="Enter category name"
                  mandatory={true}
                  value={formik.values.name}
                  onChange={formik.handleChange}
                  width="250px"
                  error={formik.touched.name && formik.errors.name}
                  disabled={disable}
                />
              </div>

              <div>
                <AutoCompleteSelect
                  width="250px"
                  name="project_id"
                  label="Project"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  value={formik.values.project_id}
                  onChange={formik.handleChange}
                  error={formik.touched.project_id && formik.errors.project_id}
                  onSelect={(value) => {
                    formik.setFieldValue('project_id', value);
                  }}
                  optionList={getAllProjectList}
                  disabled={disable}
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="budget"
                  label={inputLabelNameFromEnv}
                  placeholder="Enter budget"
                  mandatory={true}
                  value={formik.values.budget}
                  onChange={handleBudgetChange}
                  width="250px"
                  error={formik.touched.budget && formik.errors.budget}
                />
              </div>
              <div>
                <Input
                  name="label_field"
                  label={outputLableNameFromEnv}
                  placeholder="Enter budget"
                  value={appendedValue}
                  width="250px"
                  readOnly
                />
              </div>
            </div>
            <div>
              <TextArea
                name="description"
                label="Description"
                placeholder="Enter description"
                value={formik.values.description}
                onChange={formik.handleChange}
                rows={4}
                width="600px"
                maxCharacterCount={600}
                mandatory={true}
                error={formik.touched.description && formik.errors.description}
              />
            </div>
            <div className={Styles.buttonFields}>
              <div>
                <Button
                  color="secondary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={() => {
                    navigate('/settings');
                  }}
                >
                  Back
                </Button>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                >
                  Save
                </Button>
              </div>
            </div>
          </div>
        </form>
      </div>
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default CategotyAdd;
