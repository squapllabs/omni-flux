import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import Input from '../ui/Input';
import TextArea from '../ui/CustomTextArea';
import Button from '../ui/Button';
import { useFormik } from 'formik';
import { useParams } from 'react-router-dom';
import * as Yup from 'yup';
import CustomSnackBar from '../ui/customSnackBar';
import {
  getCreateValidateyup,
  getUpdateValidateyup,
} from '../../helper/constants/category/subcategory-constants';
import {
  createSubcategory,
  updateSubcategory,
} from '../../hooks/subCategory-hooks';
import { useGetAllCategoryForDrop } from '../../hooks/category-hooks';
import Select from '../ui/selectNew';
import Styles from '../../styles/categoryForm.module.scss';
import SubcategoryService from '../../service/subCategory-service';
import { formatBudgetValue } from '../../helper/common-function';
import { environment } from '../../environment/environment';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';

const SubCategoryAdd = () => {
  const { data: getAllCategoryDrop = [] } = useGetAllCategoryForDrop();
  const { mutate: createNewSubcategory } = createSubcategory();
  const { mutate: updateOneSubcategory } = updateSubcategory();
  const routeParams = useParams();

  const validationSchema =
    routeParams?.id === undefined
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);

  const [initialValues, setInitialValues] = useState({
    category_id: '',
    name: '',
    description: '',
    budget: '',
    sub_category_id: '',
  });
  const [appendedValue, setAppendedValue] = useState('');
  const inputLabelNameFromEnv = `Budget (${environment.INPUTBUDGET})`;
  const outputLableNameFromEnv = `Budget (${environment.OUTPUTBUDGET})`;
  const [disable, setDisable] = useState(
    routeParams?.id !== undefined ? true : false
  );

  const navigate = useNavigate();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');

  useEffect(() => {
    if(Number(routeParams?.id)) {
    const fetchOne = async () => {
      const data = await SubcategoryService.getOneSubcategoryByID(
        Number(routeParams?.id)
      );
      setInitialValues({
        sub_category_id: data?.data?.sub_category_id,
        name: data?.data?.name,
        budget: data?.data?.budget,
        description: data?.data?.description,
        category_id: data?.data?.category_id,
      });
      const budgetData = formatBudgetValue(Number(data?.data?.budget))
      setAppendedValue(budgetData)
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
          sub_category_id: Number(values.sub_category_id),
          name: values.name,
          budget: Number(values.budget),
          description: values.description,
          category_id: Number(values.category_id),
        };
        updateOneSubcategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success === true) {
              setMessage('Sub Category Edited');
              setOpenSnack(true);
              setTimeout(() => {navigate('/settings')},1000);
            }
          },
        });
      } else {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          description: values.description,
          category_id: Number(values.category_id),
        };
        createNewSubcategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success === true) {
              setMessage('Sub category created');
              setOpenSnack(true);
              setTimeout(() => {navigate('/settings')},1000);
              
            }
          },
        });
      }
    },
  });

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleBudgetChange = (event: any) => {
    const budgetValue = event.target.value;
    const data = formatBudgetValue(Number(budgetValue));
    setAppendedValue(data);
    formik.setFieldValue('budget', budgetValue);
    formik.handleChange(event);
  };

  return (
    <div>
      <div className={Styles.box}>
        <div>
          <h3>Sub Category Add/Edit</h3>
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
                label="Sub Category Name"
                placeholder="Enter sub category name"
                value={formik.values.name}
                mandatory={true}
                onChange={formik.handleChange}
                error={formik.touched.name && formik.errors.name}
                width="250px"
                disabled={disable}
              />
            </div>
            <div>
              <AutoCompleteSelect
                  width="250px"
                  name="category_id"
                  label="Sub Category"
                  defaultLabel="Select from options"
                  mandatory={true}
                  value={formik.values.category_id}
                  onChange={formik.handleChange}
                  disabled={disable}
                  error={formik.touched.category_id && formik.errors.category_id}
                  onSelect={(value) => {
                    formik.setFieldValue('category_id', value);
                  }}
                  optionList={getAllCategoryDrop}
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
                mandatory={true}
                maxCharacterCount={100}
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
                  type="submit"
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

export default SubCategoryAdd;
