import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import {
  createInstantCategory,
  updateCategory,
  useGetMasterAbstractStatusParentType,
} from '../../../hooks/category-hooks';
import { getAbstractValidateyup } from '../../../helper/constants/abstract-constants';
import TextArea from '../../ui/CustomTextArea';
import DatePicker from '../../ui/CustomDatePicker';
import { format } from 'date-fns';
import Select from '../../ui/selectNew';
import CategoryService from '../../../service/category-service';
import Styles from '../../../styles/newStyles/project_abstractAdd.module.scss';
import ZIcon from '../../menu/icons/zIcon';

const ProjectAbstractAdd: React.FC = (props: any) => {
  const validationSchemaAbstract = getAbstractValidateyup(Yup);
  const { mutate: createNewAbstract } = createInstantCategory();
  const { mutate: updateCategoryData } = updateCategory();
  const { data: getAllAbstractStatusDatadrop = [] } =
    useGetMasterAbstractStatusParentType();
  const [initialValues, setInitialValues] = useState<any>({
    name: props?.categoryName || '',
    description: '',
    estimated_budget:'',
    project_id: '',
    start_date: '',
    end_date: '',
    category_id: '',
    selectedBomConfig: '',
    progress_status: '',
    budget: '',
  });

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
        const data = await CategoryService.getOneCategoryByID(props.categoryId);
        setInitialValues({
          name: data?.data?.name, 
          project_id: data?.data?.project_id,
          estimated_budget : data?.data?.estimated_budget,
          description: data?.data?.description,
          start_date: dateFormat(data?.data?.start_date),
          end_date: dateFormat(data?.data?.end_date),
          category_id: data?.data?.category_id,
          progress_status: data?.data?.progress_status,
          budget: data?.data?.budget,
        });
      };
      fetchOne();
    }
  }, [props.mode, props.categoryId]);

  const formik = useFormik({
    initialValues: initialValues,
    validationSchema: validationSchemaAbstract,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (props.mode === 'EDIT') {
        const Object: any = {
          name:values.name,
          description: values.description,
          estimated_budget: values.estimated_budget,
          project_id: props.selectedProject,
          budget: initialValues.budget,
          start_date: values.start_date,
          end_date: values.end_date,
          category_id: values.category_id,
          bom_configuration_id: props.selectedBomConfig,
          progress_status: values.progress_status,
        };
        updateCategoryData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              props.setMessage('Abstract edited');
              props.setOpenSnack(true);
              resetForm();
              setInitialValues({});
              // props.setReload(true);
              props.setReload(!props.reload);

              handleClose();
            }
          },
        });
      } else {
        const Object: any = {
          name: values.name,
          description: values.description,
          estimated_budget: values.estimated_budget,
          project_id: props.selectedProject,
          budget: 0,
          start_date: values.start_date,
          end_date: values.end_date,
          bom_configuration_id: props.selectedBomConfig,
          progress_status: 'Inprogress',
        };
        createNewAbstract(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              props.setMessage('Abstract created');
              props.setOpenSnack(true);
              props.setReload(!props.reload);
              handleClose();
              resetForm();
            }
          },
        });
      }
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.divOne}>
        <div style={{ width: '70%' }}>
          {/* <div className={Styles.field}>
            <Input
              label="Name"
              placeholder="Enter abstract name"
              name="name"
              mandatory={true}
              value={formik.values.name}
              onChange={formik.handleChange}
              error={formik.touched.name && formik.errors.name}
            />
          </div> */}
          {/* {props.mode === 'EDIT' ? (
            <div className={Styles.field}>
              <Select
                label="Status"
                name="progress_status"
                mandatory={true}
                placeholder="Select the Status"
                onChange={formik.handleChange}
                value={formik.values.progress_status}
                defaultLabel="Select from options"
              >
                {getAllAbstractStatusDatadrop.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
          ) : null} */}
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
          <div className={Styles.field}>
            <Input
              label="Estimated Budget"
              placeholder="Enter Estimated Budget"
              name="estimated_budget"
              type="number"
              mandatory={true}
              value={formik.values.estimated_budget}
              onChange={formik.handleChange}
              // error={formik.touched.estimated_budget && formik.errors.estimated_budget}
            />
          </div>
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
          </div> */}
          {/* <div className={Styles.field}>
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
        <div className={Styles.icon}>
          <ZIcon width={50} height={50} />
        </div>
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
export default ProjectAbstractAdd;
