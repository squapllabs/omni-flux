import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectWorkBreakDownForm.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import TextArea from '../ui/CustomTextArea';
import Select from '../ui/selectNew';
import Button from '../ui/Button';
import {
  useGetAllParentProjectBreakDownDrop,
  getByProjectWorkBreakDownId,
  updateProjectBreakDown,
} from '../../hooks/projectBreakDown-hook';
import { editCreateValidateyup } from '../../helper/constants/projectBreakdown-constants';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import { useNavigate } from 'react-router';
import CustomSnackBar from '../ui/customSnackBar';
import { useParams } from 'react-router-dom';

const ProjectWorkBreakEdit = () => {
  const routeParams = useParams();
  const { data: getOneProjectWorkBreakDownData, isLoading } =
    getByProjectWorkBreakDownId(Number(routeParams?.id));
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const { data: getAllParentDatadrop = [] } =
    useGetAllParentProjectBreakDownDrop();
  const { data: getAllUom = [] } = useGetAllUomDrop();
  const { mutate: updateNewProjectBreakDown } = updateProjectBreakDown();
  const [initialValues, setInitialValues] = useState({
    project_workbreak_down_name: '',
    project_workbreak_down_code: '',
    parent_project_workbreak_down_id: '',
    project_workbreak_down_type: '',
    rate: '',
    uom_id: '',
    project_workbreak_down_description: '',
  });
  const navigate = useNavigate();

  const getAllProjectTypeDataForType = [
    { label: 'Custom', value: 'CUSTOM' },
    { label: 'Default', value: 'DEFAULT' },
  ];

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  useEffect(() => {
    if (getOneProjectWorkBreakDownData) {
      setInitialValues({
        project_workbreak_down_name:
          getOneProjectWorkBreakDownData?.project_workbreak_down_name || '',
        project_workbreak_down_code:
          getOneProjectWorkBreakDownData?.project_workbreak_down_code || '',
        parent_project_workbreak_down_id:
          getOneProjectWorkBreakDownData?.parent_project_workbreak_down_id ||
          '',
        project_workbreak_down_type:
          getOneProjectWorkBreakDownData?.project_workbreak_down_type || '',
        rate: getOneProjectWorkBreakDownData?.rate || '',
        uom_id: getOneProjectWorkBreakDownData?.uom_id || '',
        project_workbreak_down_description:
          getOneProjectWorkBreakDownData?.project_workbreak_down_description ||
          '',
      });
    }
  }, [getOneProjectWorkBreakDownData]);

  const validationSchema = editCreateValidateyup(Yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      const Object: any = {
        project_workbreak_down_name: values.project_workbreak_down_name,
        project_workbreak_down_code: values.project_workbreak_down_code,
        parent_project_workbreak_down_id:
          Number(values.parent_project_workbreak_down_id) === 0
            ? null
            : Number(values.parent_project_workbreak_down_id),
        project_workbreak_down_type: values.project_workbreak_down_type,
        rate: Number(values.rate),
        uom_id: Number(values.uom_id),
        project_workbreak_down_description:
          values.project_workbreak_down_description,
        project_workbreak_down_id: Number(routeParams?.id),
      };
      updateNewProjectBreakDown(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('Project Workbreak down edited');
            setOpenSnack(true);
            setInterval(() => {
              navigate('/project-workbreakdown');
            }, 1000);
          }
        },
      });
    },
  });
  if (isLoading) {
    return <div>Loading...</div>;
  }
  return (
    <div className={Styles.container}>
      <div className={Styles.textContent}>
        <h3>Edit - Work Break Down</h3>
        <span className={Styles.content}>Edit your work break down</span>
      </div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.inputFieldMain}>
          <div className={Styles.inputFields}>
            <div style={{ width: '40%' }}>
              <Input
                label="Work breakdown name *"
                placeholder="Enter work breakdown name"
                name="project_workbreak_down_name"
                value={formik.values.project_workbreak_down_name}
                onChange={formik.handleChange}
                error={
                  formik.touched.project_workbreak_down_name &&
                  formik.errors.project_workbreak_down_name
                }
              />
            </div>
            <div style={{ width: '40%' }}>
              <Input
                label="Work breakdown code (Read Only)"
                placeholder="Enter work breakdown code"
                name="project_workbreak_down_code"
                value={formik.values.project_workbreak_down_code}
                onChange={formik.handleChange}
                error={
                  formik.touched.project_workbreak_down_code &&
                  formik.errors.project_workbreak_down_code
                }
                disabled={true}
              />
            </div>
          </div>
          <div className={Styles.inputFields}>
            <div style={{ width: '40%' }}>
              <Select
                label="Parent work breakdown"
                name="parent_project_workbreak_down_id"
                onChange={formik.handleChange}
                value={formik.values.parent_project_workbreak_down_id}
                defaultLabel="Select from options"
              >
                {getAllParentDatadrop.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div style={{ width: '40%' }}>
              <Select
                label="Work breakdown type (Read Only)"
                name="project_workbreak_down_type"
                onChange={formik.handleChange}
                value={formik.values.project_workbreak_down_type}
                defaultLabel="Select from options"
                error={
                  formik.touched.project_workbreak_down_type &&
                  formik.errors.project_workbreak_down_type
                }
                disabled={true}
              >
                {getAllProjectTypeDataForType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
          </div>
          <div className={Styles.inputFields}>
            <div style={{ width: '40%' }}>
              <Input
                label="Rate"
                placeholder="Enter rate"
                name="rate"
                value={formik.values.rate}
                onChange={formik.handleChange}
                error={formik.touched.rate && formik.errors.rate}
              />
            </div>
            <div style={{ width: '40%' }}>
              <Select
                label="UOM"
                name="uom_id"
                onChange={formik.handleChange}
                value={formik.values.uom_id}
                defaultLabel="Select from options"
              >
                {getAllUom.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
          </div>
          <div className={Styles.inputTextArea}>
            <div className={Styles.inputFieldsArea}>
              <TextArea
                name="project_workbreak_down_description"
                label="Description"
                placeholder="Enter description"
                value={formik.values.project_workbreak_down_description}
                onChange={formik.handleChange}
                rows={3}
                maxCharacterCount={120}
              />
            </div>
          </div>
          <div className={Styles.submitButton}>
            <Button
              className={Styles.resetButton}
              type="submit"
              shape="rectangle"
              justify="center"
              onClick={() => navigate('/project-workbreakdown')}
            >
              Back
            </Button>
            <Button
              type="submit"
              color="primary"
              shape="rectangle"
              justify="center"
            >
              Submit
            </Button>
          </div>
        </div>
      </form>
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

export default ProjectWorkBreakEdit;
