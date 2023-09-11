import React, { useState } from 'react';
import Button from '../../ui/Button';
import Styles from '../../../styles/project.module.scss';
import { useFormik } from 'formik';
import * as yup from 'yup';
import Input from '../../ui/Input';
import DatePicker from '../../ui/CustomDatePicker';
import AddIcon from '../../menu/icons/addIcon';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { useGetAllClientDrop } from '../../../hooks/client-hooks';
import { useGetAllUsersDrop, useGetAllUsers } from '../../../hooks/user-hooks';
import { useGetAllSiteDrops } from '../../../hooks/site-hooks';
import {
  createProject,
  useGetMasterProjectParentType,
} from '../../../hooks/project-hooks';
import CustomClientAdd from '../../ui/CustomClientAdd';
import CustomSiteAdd from '../../ui/CustomSiteAdd';
import CustomConfirm from '../../ui/CustomConfirmDialogBox';
import TextArea from '../../ui/CustomTextArea';
import Select from '../../ui/selectNew';

const ProjectGeneralDetails: React.FC = (props: any) => {
  const currentDate = new Date();
  const defaultEndDate = new Date();
  defaultEndDate.setDate(currentDate.getDate() + 90);
  const [initialValues, setInitialValues] = useState({
    project_name: '',
    code: '',
    user_id: '',
    client_id: '',
    date_started: currentDate.toISOString().slice(0, 10),
    date_ended: defaultEndDate.toISOString().slice(0, 10),
    project_type: '',
    approvar_id: '',
    estimated_budget: '',
    actual_budget: '',
    description: '',
    project_notes: '',
    site_configuration: '',
    project_documents: '',
    status: '',
    submitType: '',
  });
  const [showClientForm, setShowClientForm] = useState(false);
  const [showSiteForm, setShowSiteForm] = useState(false);
  const [openConfirm, setOpenConfirm] = useState(false);
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const { data: getAllUsersSiteDatadrop = [] } = useGetAllUsers();
  const { data: getAllClientDatadrop = [] } = useGetAllClientDrop();
  const { data: getAllProjectTypeDatadrop = [] } =
    useGetMasterProjectParentType();
  const handleOpenClientForm = () => {
    setShowClientForm(true);
  };
  const submitHandler = () => {
    setOpenConfirm(true);
  };
  const handleCloseConfirm = () => {
    setOpenConfirm(false);
  };
  const handleConfirmForm = () => {
    formik.setFieldValue('submitType', 'Inprogress');
    formik.submitForm();
    setOpenConfirm(false);
  };
  const drafthandler = () => {
    formik.setFieldValue('submitType', 'Draft');
    formik.submitForm();
  };
  const formik = useFormik({
    initialValues,
    // validationSchema: validateSchema,
    onSubmit: async (values) => {
      console.log('values', values);
    },
  });
  return (
    <div>
      <div>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.inputFieldMain}>
            <div className={Styles.inputFields}>
              <div style={{ width: '40%' }}>
                <Input
                  label="Name"
                  placeholder="Enter project name"
                  name="project_name"
                  mandatory={true}
                  value={formik.values.project_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.project_name && formik.errors.project_name
                  }
                />
              </div>
              <div style={{ width: '40%' }}>
                <Input
                  label="Code"
                  placeholder="Enter project code"
                  name="code"
                  mandatory={true}
                  value={formik.values.code}
                  onChange={formik.handleChange}
                  error={formik.touched.code && formik.errors.code}
                />
              </div>
            </div>
            <div className={Styles.inputFields}>
              <div style={{ width: '40%' }}>
                <AutoCompleteSelect
                  name="user_id"
                  label="Project Manager"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  value={formik.values.user_id}
                  onChange={formik.handleChange}
                  error={formik.touched.user_id && formik.errors.user_id}
                  onSelect={(value) => {
                    formik.setFieldValue('user_id', value);
                  }}
                  // disabled={disable}
                  optionList={getAllUsersDatadrop}
                />
              </div>
              <div style={{ width: '40%' }} className={Styles.client}>
                <AutoCompleteSelect
                  name="client_id"
                  label="Client / Customer"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  value={formik.values.client_id}
                  onChange={formik.handleChange}
                  error={formik.touched.client_id && formik.errors.client_id}
                  onSelect={(value) => {
                    formik.setFieldValue('client_id', value);
                  }}
                  // disabled={disable}
                  optionList={getAllClientDatadrop}
                />
                <div
                  className={Styles.instantAdd}
                  onClick={handleOpenClientForm}
                >
                  <AddIcon style={{ height: '15px', width: '15px' }} />
                  <h4 className={Styles.addtext}>Add client</h4>
                </div>
              </div>
            </div>
            <div className={Styles.inputFields}>
              <div style={{ width: '40%' }}>
                <DatePicker
                  label="Start Date"
                  name="date_started"
                  mandatory={true}
                  value={formik.values.date_started}
                  onChange={formik.handleChange}
                  InputProps={{
                    inputProps: {
                      min: '1930-01-01',
                      max: `${new Date().toISOString().slice(0, 10)}`,
                    },
                  }}
                  error={
                    formik.touched.date_started && formik.errors.date_started
                  }
                />
              </div>
              <div style={{ width: '40%' }}>
                <DatePicker
                  label="End Date"
                  name="date_ended"
                  mandatory={true}
                  value={formik.values.date_ended}
                  onChange={formik.handleChange}
                  InputProps={{
                    inputProps: {
                      min: '1930-01-01',
                      max: `${new Date().toISOString().slice(0, 10)}`,
                    },
                  }}
                  error={formik.touched.date_ended && formik.errors.date_ended}
                />
              </div>
            </div>
            <div className={Styles.inputFields}>
              <div style={{ width: '40%' }}>
                <Select
                  label="Project Type"
                  name="project_type"
                  mandatory={true}
                  onChange={formik.handleChange}
                  value={formik.values.project_type}
                  defaultLabel="Select from options"
                  error={
                    formik.touched.project_type && formik.errors.project_type
                  }
                >
                  {getAllProjectTypeDatadrop.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </Select>
              </div>
              <div style={{ width: '40%' }}>
                <AutoCompleteSelect
                  name="approvar_id"
                  label="Approver"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  value={formik.values.approvar_id}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.approvar_id && formik.errors.approvar_id
                  }
                  onSelect={(value) => {
                    formik.setFieldValue('approvar_id', value);
                  }}
                  // disabled={disable}
                  optionList={getAllUsersDatadrop}
                />
              </div>
            </div>
            <div className={Styles.inputFields}>
              <div style={{ width: '40%' }}>
                <Input
                  label="Estimated Budget"
                  placeholder="Enter rate"
                  name="estimated_budget"
                  mandatory={true}
                  onChange={formik.handleChange}
                  value={formik.values.estimated_budget}
                  error={
                    formik.touched.estimated_budget &&
                    formik.errors.estimated_budget
                  }
                />
              </div>
              <div style={{ width: '40%' }}>
                <Input
                  label="Actual Budget"
                  placeholder="Enter rate"
                  name="actual_budget"
                  onChange={formik.handleChange}
                  value={formik.values.actual_budget}
                  error={
                    formik.touched.actual_budget && formik.errors.actual_budget
                  }
                />
              </div>
            </div>
            <div className={Styles.inputFields}>
              <div style={{ width: '40%' }}>
                <TextArea
                  name="description"
                  label="Project Description"
                  placeholder="Enter project description"
                  value={formik.values.description}
                  onChange={formik.handleChange}
                  rows={4}
                  maxCharacterCount={100}
                />
              </div>
              <div style={{ width: '40%' }}>
                <TextArea
                  name="project_notes"
                  label="Project Notes"
                  placeholder="Enter project notes"
                  value={formik.values.project_notes}
                  onChange={formik.handleChange}
                  rows={4}
                  maxCharacterCount={100}
                />
              </div>
            </div>
          </div>
        </form>
      </div>
      <div className={Styles.submitButton}>
        <Button
          className={Styles.resetButton}
          type="button"
          shape="rectangle"
          size="small"
          justify="center"
          onClick={() => drafthandler()}
        >
          Draft
        </Button>
        <Button
          type="button"
          color="primary"
          shape="rectangle"
          size="small"
          justify="center"
          onClick={() => submitHandler()}
        >
          Submit
        </Button>
      </div>

      {/* <Button
        shape="rectangle"
        justify="center"
        size="small"
        onClick={(e) => props.setActiveButton('PSC')}
      >
        SAVE & MOVE ON
      </Button> */}
      <div>
        <CustomClientAdd
          isVissible={showClientForm}
          onAction={setShowClientForm}
        />
        <CustomSiteAdd
          isVissiblesite={showSiteForm}
          onActionsite={setShowSiteForm}
        />
        <CustomConfirm
          open={openConfirm}
          title="Confirm Submit"
          contentLine1="If you confirmed this project it will move to the review process"
          handleClose={handleCloseConfirm}
          handleConfirm={handleConfirmForm}
        />
      </div>
    </div>
  );
};

export default ProjectGeneralDetails;
