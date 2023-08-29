import React, { useState, useEffect } from 'react';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import { useGetAllSubcategoryDrop } from '../../hooks/subCategory-hooks';
import {
  getCreateValidateyup,
  getUpdateValidateyup,
} from '../../helper/constants/category/subsubcategory-constants';
import {
  createSubSubcategory,
  updateSubSubcategory,
  getBySubSubcategoryID,
} from '../../hooks/subSubCategory-hooks';
import * as Yup from 'yup';
import { useFormik } from 'formik';
import Select from '../ui/selectNew';
import CustomSnackBar from '../ui/customSnackBar';
import { useNavigate } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import Styles from '../../styles/categoryForm.module.scss';
import TextArea from '../ui/CustomTextArea';
import SubSubCategoryService from '../../service/subSubCategory-service';
import { formatBudgetValue } from '../../helper/common-function';
import { environment } from '../../environment/environment';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';

const SubsubCategoryAdd = () => {
  const { data: getAllSubCategory = [] } = useGetAllSubcategoryDrop();
  const { mutate: createNewSubSubCategory } = createSubSubcategory();
  const { mutate: updateSubSubCategory } = updateSubSubcategory();
  const routeParams = useParams();
  const validationSchema =
    routeParams?.id === undefined
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  // const { data: getOneSubSubCategoryData } = getBySubSubcategoryID(
  //   Number(routeParams?.id)
  // );
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [appendedValue, setAppendedValue] = useState('');
  const inputLabelNameFromEnv = `Budget (${environment.INPUTBUDGET})`;
  const outputLableNameFromEnv = `Budget (${environment.OUTPUTBUDGET})`;
  const [initialValues, setInitialValues] = useState({
    sub_sub_category_id: '',
    name: '',
    budget: '',
    description: '',
    sub_category_id: '',
  });
  const [disable, setDisable] = useState(
    routeParams?.id !== undefined ? true : false
  );

  const navigate = useNavigate();

  useEffect(() => {
    if (Number(routeParams?.id)) {
      const fetchOne = async () => {
        const data = await SubSubCategoryService.getOneSubSubcategoryByID(
          Number(routeParams?.id)
        );
        setInitialValues({
          sub_sub_category_id: data?.data?.sub_sub_category_id,
          name: data?.data?.name,
          budget: data?.data?.budget,
          description: data?.data?.description,
          sub_category_id: data?.data?.sub_category_id,
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
    onSubmit: (values, { resetForm }) => {
      if (Number(routeParams?.id)) {
        const Object: any = {
          sub_sub_category_id: values.sub_sub_category_id,
          name: values.name,
          description: values.description,
          budget: Number(values.budget),
          sub_category_id: Number(values.sub_category_id),
        };
        updateSubSubCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setOpenSnack(true);
              setMessage('Sub Sub Category Edited');
              resetForm();
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
          sub_category_id: Number(values.sub_category_id),
        };
        createNewSubSubCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setOpenSnack(true);
              setMessage('Sub Sub Category created');
              resetForm();
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
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
          <h3>Sub Sub Category Add/Edit</h3>
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
                  label="Sub Sub Category Name"
                  placeholder="Sub sub category name"
                  mandatory={true}
                  width="250px"
                  value={formik.values.name}
                  onChange={formik.handleChange}
                  error={formik.touched.name && formik.errors.name}
                  disabled={disable}
                />
              </div>
              <div>
                <AutoCompleteSelect
                  width="250px"
                  name="sub_category_id"
                  label="Sub Sub Category"
                  defaultLabel="Select from options"
                  mandatory={true}
                  value={formik.values.sub_category_id}
                  onChange={formik.handleChange}
                  disabled={disable}
                  error={
                    formik.touched.sub_category_id &&
                    formik.errors.sub_category_id
                  }
                  onSelect={(value) => {
                    formik.setFieldValue('sub_category_id', value);
                  }}
                  optionList={getAllSubCategory}
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
export default SubsubCategoryAdd;
