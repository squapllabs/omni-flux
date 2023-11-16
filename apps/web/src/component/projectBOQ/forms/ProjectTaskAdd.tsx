import React, { useEffect, useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import { useCreateInstantSubcategory } from '../../../hooks/subCategory-hooks';
import { getSubCategoryValidateyup } from '../../../helper/constants/abstract-constants';
import TextArea from '../../ui/CustomTextArea';
import DatePicker from '../../ui/CustomDatePicker';
import SubcategoryService from '../../../service/subCategory-service';
import { format } from 'date-fns';
import Styles from '../../../styles/newStyles/project_abstractAdd.module.scss';
import CheckListIcon from '../../menu/icons/checkListIcon';

import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { useGetUomByType } from '../../../hooks/uom-hooks';

const ProjectTaskAdd: React.FC = (props: any) => {
  console.log('props', props?.isCollapsed);
  const validationSchemaSubCategory = getSubCategoryValidateyup(Yup);
  const { mutate: createNewSubCategory } = useCreateInstantSubcategory();
  const { mutate: useCreateInstantSubcategoryData } =
    useCreateInstantSubcategory();
  const [initialValues, setInitialValues] = useState({
    name: '',
    description: '',
    project_id: '',
    category_id: '',
    start_date: '',
    end_date: '',
    sub_category_id: '',
    actual_budget: '',
    parent_sub_category_id: '',
    estimated_budget: 0,
    uom_id: null,
    rate: null,
    quantity: null,
  });

  const DropfieldWidth = '150px';
  const { data: getAllUomDrop } = useGetUomByType('RAWMT');

  const dateFormat = (value: any) => {
    if (value !== null) {
      const currentDate = new Date(value);
      const formattedDate = format(currentDate, 'yyyy-MM-dd');
      return formattedDate;
    }
  };
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await SubcategoryService.getOneSubcategoryByID(
          props.selectedSubCategory
        );
        setInitialValues({
          name: data?.data?.name,
          description: data?.data?.description,
          start_date: dateFormat(data?.data?.start_date),
          end_date: dateFormat(data?.data?.end_date),
          category_id: data?.data?.category_id,
          project_id: data?.data?.project_id,
          sub_category_id: data?.data?.sub_category_id,
          bom_configuration_id: props.selectedBomConfig,
          actual_budget: data?.data?.actual_budget,
          parent_sub_category_id: data?.data?.parent_sub_category_id,
          estimated_budget: data?.data?.estimated_budget,
          uom_id: data?.data?.uom_id,
          rate: data?.data?.rate,
          quantity: data?.data?.quantity,
        });
      };
      if (props.mode === 'EDIT') fetchOne();
    }
  }, [props?.mode]);

  const formik = useFormik({
    initialValues: initialValues,
    validationSchema: validationSchemaSubCategory,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (props.mode === 'EDIT') {
        const Object: any = {
          name: values.name,
          description: values.description,
          project_id: props.selectedProject,
          actual_budget: initialValues.actual_budget,
          category_id: props.selectedCategoryId,
          start_date: values.start_date,
          end_date: values.end_date,
          sub_category_id: values.sub_category_id,
          parent_sub_category_id: values.parent_sub_category_id,
          estimated_budget: values.estimated_budget,
          uom_id: values.uom_id || null,
          rate: values.rate || null,
          quantity: values.quantity || null,
        };
        console.log('abstract from', Object);
        useCreateInstantSubcategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              resetForm();
              props.setMessage('Task edited');
              props.setOpenSnack(true);
              props.setOpen(false);
              props.setReload(!props.reload);
              props.setSubTaskView(!props.subTaskView);
              props.setIsCollapsed(!props.isCollapsed);
            }
          },
        });
      } else {
        const Object: any = {
          name: values.name,
          description: values.description,
          project_id: props.selectedProject,
          actual_budget: 0,
          category_id: props.selectedCategoryId,
          start_date: values.start_date,
          end_date: values.end_date,
          bom_configuration_id: props.selectedBomConfig,
          parent_sub_category_id:
            props.mode === 'Sub Task' ? props.selectedSubCategory : null,
          estimated_budget: values.estimated_budget,
          uom_id: values.uom_id,
          rate: values.rate,
          quantity: values.quantity,
        };
        createNewSubCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              props.setMessage('Task created');
              props.setOpenSnack(true);
              props.setOpen(false);
              console.log('data_created', data);
              props.setReload(!props.reload);
              resetForm();
              props.setIsCollapsed(!props.isCollapsed);
              props.setSubTaskView(!props.subTaskView);
            }
          },
        });
      }
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  };

  const calculatebudget = () => {
    let estimated_budget =
      Number(formik.values.rate) * Number(formik.values.quantity);

    if (Number.isNaN(estimated_budget)) {
      estimated_budget = 0;
    }
    formik.values.estimated_budget = estimated_budget;
    return estimated_budget;
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.divOne}>
        <div style={{ width: '70%' }}>
          {/* <div className={Styles.field}>
            <Input
              label="Name"
              placeholder="Enter task name"
              name="name"
              mandatory={true}
              value={formik.values.name}
              onChange={formik.handleChange}
              error={formik.touched.name && formik.errors.name}
            />
          </div> */}
          <div className={Styles.field}>
            <TextArea
              name="description"
              label="Description"
              placeholder="Enter description"
              value={formik.values.description}
              onChange={formik.handleChange}
              mandatory={true}
              error={formik.touched.description && formik.errors.description}
              rows={10}
              maxCharacterCount={1000}
            />
          </div>
        </div>
        <div className={Styles.icon}>
          <CheckListIcon width={50} height={50} />
        </div>
      </div>
      <div className={Styles.row_container}>
        <div>
          <AutoCompleteSelect
            width={DropfieldWidth}
            name="uom_id"
            label="Select UOM"
            mandatory={false}
            optionList={getAllUomDrop != undefined ? getAllUomDrop : []}
            value={formik.values.uom_id}
            onChange={formik.handleChange}
            onSelect={(e: string): void => {
              formik.values.uom_id = e;
            }}
            defaultLabel="Select"
            placeholder="Select"
          />
          {formik.touched.uom_id}
        </div>
        <div className={Styles.field}>
          <Input
            label="Quantity"
            placeholder="Enter Quantity"
            name="quantity"
            type="number"
            mandatory={false}
            value={formik.values.quantity}
            onChange={formik.handleChange}
            width={DropfieldWidth}
            error={formik.touched.quantity && formik.errors.quantity}
          />
        </div>
        <div className={Styles.field}>
          <Input
            label="Rate"
            placeholder="Enter Rate"
            name="rate"
            type="number"
            mandatory={false}
            value={formik.values.rate}
            onChange={formik.handleChange}
            width={DropfieldWidth}
            error={formik.touched.rate && formik.errors.rate}
          />
        </div>
        <div className={Styles.field}>
          <Input
            label="Estimated Budget"
            placeholder=""
            name="estimated_budget"
            type="number"
            mandatory={false}
            value={calculatebudget()}
            onChange={formik.handleChange}
            width={DropfieldWidth}
          />
        </div>
        {/* <div className={Styles.total_budget}>
          Estimated Budget : {calculatebudget()}
        </div>       */}
        {/* <div className={Styles.field}>
            <DatePicker
              label="Start Date"
              name="start_date"
              value={formik.values.start_date}
              onChange={formik.handleChange}
              InputProps={{
                inputProps: {
                  min: '1930-01-01',
                  max: `${new Date().toISOString().slice(0, 10)}`,
                },
              }}
              error={formik.touched.start_date && formik.errors.start_date}
              width="200px"
            />
          </div>
          <div className={Styles.field}>
            <DatePicker
              label="End Date"
              name="end_date"
              value={formik.values.end_date}
              onChange={formik.handleChange}
              error={formik.touched.end_date && formik.errors.end_date}
              width="200px"
            />
          </div> */}
      </div>
      <div className={Styles.sub_sub_container_2}>
        <div className={Styles.footer}>
          <div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.button}>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                onClick={handleClose}
                className={Styles.cancelButton}
              >
                Cancel
              </Button>
              <Button
                shape="rectangle"
                color="primary"
                justify="center"
                size="small"
                type="submit"
                onClick={formik.handleSubmit}
              >
                Save
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
export default ProjectTaskAdd;
