import React, { useState, useEffect } from 'react';
import Button from '../../ui/Button';
import Styles from '../../../styles/projectDetails.module.scss';
import { useFormik } from 'formik';
import * as yup from 'yup';
import Input from '../../ui/Input';
import DatePicker from '../../ui/CustomDatePicker';
import ProjectDetailsIcon from '../../menu/icons/projectDetailsIcon';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { useGetAllClientDrop } from '../../../hooks/client-hooks';
import { useGetUserbyRole } from '../../../hooks/user-hooks';
import {
  useCreateProject,
  useGetMasterProjectParentType,
  useUpdateProject,
} from '../../../hooks/project-hooks';
import CustomClientAdd from '../../ui/CustomClientAdd';
import TextArea from '../../ui/CustomTextArea';
import Select from '../../ui/selectNew';
import projectService from '../../../service/project-service';
import { useParams, useNavigate } from 'react-router-dom';
import CustomSnackBar from '../../ui/customSnackBar';
import CustomSidePopup from '../../ui/CustomSidePopup';
import { store, RootState } from '../../../redux/store';
import { getToken } from '../../../redux/reducer';
import { format } from 'date-fns';

const ProjectGeneralDetails: React.FC = (props: any) => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const routeParams = useParams();
  const navigate = useNavigate();
  const currentDate = new Date();
  const defaultEndDate = new Date();
  defaultEndDate.setDate(currentDate.getDate() + 90);
  const [initialValues, setInitialValues] = useState({
    project_name: '',
    code: '',
    user_id: roleName === 'PROJECT MANAGER' ? userID : '',
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
    bom_configuration: '',
    project_documents: '',
    status: '',
    submitType: '',
    project_estimated_budget: '',
    project_id: '',
  });
  const [showClientForm, setShowClientForm] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [bomConfig, setBomConfig] = useState<any>([]);
  const [siteConfigData, setSiteConfigData] = useState<any[]>([]);
  const { data: getAllClientDatadrop = [] } = useGetAllClientDrop();
  const { data: getProjectManagerList = [] } =
    useGetUserbyRole('Project Manager');
  const { data: getProjectApproverList = [] } =
    useGetUserbyRole('Planning Engineer');
  const { data: getAllProjectTypeDatadrop = [] } =
    useGetMasterProjectParentType();
  const { mutate: createNewProjectData } = useCreateProject();
  const { mutate: updateProjectData } = useUpdateProject();
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  /* Fnction to get project details by ID */
  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectService.getOneProjectById(
        Number(routeParams?.id)
      );
      setInitialValues({
        project_name: getData?.data?.project_name,
        code: getData?.data?.code,
        user_id: getData?.data?.user_id,
        client_id: getData?.data?.client_id,
        date_started: dateFormat(getData?.data?.date_started),
        date_ended: dateFormat(getData?.data?.date_ended),
        project_type: getData?.data?.project_type,
        approvar_id: getData?.data?.approvar_id,
        estimated_budget: getData?.data?.estimated_budget,
        actual_budget: getData?.data?.actual_budget,
        description: getData?.data?.description,
        project_notes: getData?.data?.project_notes,
        site_configuration: getData?.data?.site_configuration,
        project_documents: getData?.data?.project_documents,
        status: getData?.data?.status,
        submitType: getData?.data?.submitType,
        project_estimated_budget: '',
        project_id: getData?.data?.project_id,
        bom_configuration: getData?.data?.bom_configuration,
      });
      setBomConfig(getData?.data?.bom_configuration);
      setSiteConfigData(getData?.data?.project_site);
    };
    if (routeParams?.id !== undefined) fetchData();
  }, [routeParams?.id]);
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const cancelhandler = () => {
    navigate('/project-list');
  };
  const handleClientFormClose = () => {
    setShowClientForm(false);
  };
  const validateSchemaCreate = yup.object().shape({
    project_name: yup.string().required('Project name is required'),
    code: yup
      .string()
      .required('Project code is required')
      .test(
        'code-availability',
        'Code is already present',
        async (value: any) => {
          if (value) {
            const response = await projectService.checkProjectCodeDuplicate(
              value
            );
            return !response?.is_exist;
          }
          return true;
        }
      ),
    user_id: yup.string().trim().required('Project manager is required'),
    approvar_id: yup.string().trim().required('Approver is required'),
    client_id: yup
      .string()
      .trim()
      .required('Project client/customer is required'),
    estimated_budget: yup
      .number()
      .min(1, 'Value must be greater than 0')
      .max(5000000000, 'Value must be less then 5000000000')
      .typeError('Only Number are allowed')
      .required('Estimated budget is required'),
    actual_budget: yup
      .number()
      .min(1, 'Value must be greater than 0')
      .max(5000000000, 'Value must be less then 5000000000')
      .typeError('Only Number are allowed'),
    project_type: yup.string().trim().required('Project type is required'),
    date_started: yup.date().required('Project start date is required'),
    date_ended: yup
      .date()
      .required('Project end date is required')
      .min(
        yup.ref('date_started'),
        'Project end date cannot be earlier than start date'
      ),
  });
  const validateSchemaEdit = yup.object().shape({
    project_id: yup.number().required(),
    project_name: yup.string().required('Project name is required'),
    user_id: yup.string().trim().required('Project manager is required'),
    approvar_id: yup.string().trim().required('Approver is required'),
    client_id: yup
      .string()
      .trim()
      .required('Project client/customer is required'),
    code: yup
      .string()
      .required('Project code is required')
      .test(
        'code-availability',
        'Code is already present',
        async (value: any, { parent }: yup.TestContext) => {
          const ProjectId = parent.project_id;
          if (value) {
            const response = await projectService.checkProjectCodeDuplicate(
              value
            );
            if (
              response?.is_exist === true &&
              response?.data?.project_id === ProjectId
            ) {
              return true;
            } else {
              if (response?.is_exist === false) {
                return true;
              } else {
                if (response?.is_exist === false) {
                  return true;
                } else {
                  return false;
                }
              }
            }
          }
        }
      ),
    estimated_budget: yup
      .number()
      .min(1, 'Value must be greater than 0')
      .max(5000000000, 'Value must be less then 5000000000')
      .typeError('Only Number are allowed')
      .required('Estimated budget is required'),
    actual_budget: yup
      .number()
      .max(5000000000, 'Value must be less then 5000000000')
      .typeError('Only Number are allowed'),
    project_type: yup.string().trim().required('Project type is required'),
    date_started: yup.date().required('Project start date is required'),
    date_ended: yup
      .date()
      .required('Project end date is required')
      .min(
        yup.ref('date_started'),
        'Project end date cannot be earlier than start date'
      ),
  });
  /* Function to create and update project general details */
  const formik = useFormik({
    initialValues,
    validationSchema:
      routeParams?.id === undefined ? validateSchemaCreate : validateSchemaEdit,
    enableReinitialize: true,
    onSubmit: async (values) => {
      const user_id =
        roleName === 'PROJECT MANAGER' ? userID : Number(values.user_id);
      const Object: any = {
        project_id: values.project_id,
        project_name: values.project_name,
        code: values.code.toUpperCase(),
        user_id: user_id,
        client_id: Number(values.client_id),
        date_started: values.date_started,
        date_ended: values.date_ended,
        project_type: values.project_type,
        approvar_id: Number(values.approvar_id),
        estimated_budget: Number(values.estimated_budget),
        actual_budget: Number(values.actual_budget),
        description: values.description,
        project_notes: values.project_notes,
        site_configuration: siteConfigData,
        project_documents: [],
        bom_configuration: bomConfig,
        status: 'Inprogress',
      };
      if (routeParams?.id === undefined) {
        createNewProjectData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setMessage('Project created');
              setOpenSnack(true);
              props.setLoader(!props.loader);
              if (data?.data?.project?.status === 'Draft') {
                setTimeout(() => {
                  navigate('/project-list');
                }, 1000);
              } else {
                setTimeout(() => {
                  navigate(`/project-edit/${data?.data?.project?.project_id}`);
                  props.setLoader(props.loader);
                }, 1000);
              }
            }
          },
        });
      } else {
        updateProjectData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setMessage('Project updated');
              setOpenSnack(true);
              props.setLoader(!props.loader);
              setTimeout(() => {
                navigate(`/project-edit/${data?.data?.project?.project_id}`);
                props.setLoader(props.loader);
              }, 1000);
            }
          },
        });
      }
    },
  });

  return (
    <div>
      <div className={Styles.topHeading}>
        <div className={Styles.subHeading}>
          <ProjectDetailsIcon />
          <h3>PROJECT DETAILS </h3>
        </div>
      </div>
      <div>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.childTwo}>
            <div className={Styles.subChildOne}>
              <div className={Styles.subOneChildOne}>
                <div className={Styles.inputFields}>
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
                <div className={Styles.inputFields}>
                  <Select
                    label="Project Type"
                    name="project_type"
                    mandatory={true}
                    onChange={formik.handleChange}
                    value={formik.values.project_type}
                    defaultLabel="Select from options"
                    placeholder="Select from options"
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
              </div>
              <div className={Styles.subOneChildTwo}>
                <div className={Styles.inputFields}>
                  <Input
                    label="Code"
                    placeholder="Enter project code"
                    name="code"
                    mandatory={true}
                    value={formik.values.code.toUpperCase()}
                    onChange={formik.handleChange}
                    error={formik.touched.code && formik.errors.code}
                  />
                </div>
                <div className={Styles.clientInstantAdd}>
                  <div style={{ width: '80%' }}>
                    <AutoCompleteSelect
                      name="client_id"
                      label="Client / Customer"
                      defaultLabel="Select from options"
                      placeholder="Select from options"
                      mandatory={true}
                      value={formik.values.client_id}
                      onChange={formik.handleChange}
                      error={
                        formik.touched.client_id && formik.errors.client_id
                      }
                      onSelect={(value) => {
                        formik.setFieldValue('client_id', value);
                      }}
                      addLabel="Add Client"
                      onAddClick={(value) => {
                        setShowClientForm(true);
                      }}
                      optionList={getAllClientDatadrop}
                    />
                  </div>
                </div>
              </div>
              <div className={Styles.subOneChildThree}>
                <div className={Styles.inputFields}>
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
                    disabled={roleName === 'PROJECT MANAGER'}
                    optionList={getProjectManagerList}
                  />
                </div>
                <div className={Styles.inputFields}>
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
                    optionList={getProjectApproverList}
                  />
                </div>
              </div>
            </div>
            <div className={Styles.dividerStyleOne}></div>
            <div className={Styles.subChildTwo}>
              <div className={Styles.subTwoChildOne}>
                <div style={{ width: '40%'}}>
                  <DatePicker
                    width="55%"
                    label="Start Date"
                    name="date_started"
                    mandatory={true}
                    value={formik.values.date_started}
                    onChange={formik.handleChange}
                    min="1930-01-01"
                    max={new Date().toISOString().slice(0, 10)}
                    // InputProps={{
                    //   inputProps: {
                    //     min: '1930-01-01',
                    //     max: `${new Date().toISOString().slice(0, 10)}`,
                    //   },
                    // }}
                    error={
                      formik.touched.date_started && formik.errors.date_started
                    }
                  />
                </div>
                <div style={{ width: '40%' }}>
                  <DatePicker
                    width="55%"
                    label="End Date"
                    name="date_ended"
                    mandatory={true}
                    value={formik.values.date_ended}
                    onChange={formik.handleChange}
                    min="1930-01-01"
                    max={new Date().toISOString().slice(0, 10)}
                    // InputProps={{
                    //   inputProps: {
                    //     min: '1930-01-01',
                    //     max: `${new Date().toISOString().slice(0, 10)}`,
                    //   },
                    // }}
                    error={
                      formik.touched.date_ended && formik.errors.date_ended
                    }
                  />
                </div>
              </div>
            </div>
            <div className={Styles.dividerStyleOne}></div>
            <div className={Styles.subChildThree}>
              <div className={Styles.subThreeChildOne}>
                <div className={Styles.inputFields}>
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
                <div className={Styles.inputFields}>
                  <Input
                    label="Actual Budget"
                    placeholder="Enter rate"
                    name="actual_budget"
                    onChange={formik.handleChange}
                    value={formik.values.actual_budget}
                    error={
                      formik.touched.actual_budget &&
                      formik.errors.actual_budget
                    }
                  />
                </div>
              </div>
            </div>
            <div className={Styles.dividerStyleOne}></div>
            <div className={Styles.subChildFour}>
              <div className={Styles.subFourChildOne}>
                <div className={Styles.inputFields}>
                  <TextArea
                    name="description"
                    label="Project Description"
                    placeholder="Enter project description"
                    value={formik.values.description}
                    onChange={formik.handleChange}
                    rows={3}
                    maxCharacterCount={100}
                  />
                </div>
                <div className={Styles.inputFields}>
                  <TextArea
                    name="project_notes"
                    label="Project Notes"
                    placeholder="Enter project notes"
                    value={formik.values.project_notes}
                    onChange={formik.handleChange}
                    rows={3}
                    maxCharacterCount={100}
                  />
                </div>
              </div>
            </div>
            <div className={Styles.dividerStyleOne}></div>
            <div className={Styles.bottomButton}>
              <Button
                type="button"
                color="secondary"
                shape="rectangle"
                size="small"
                justify="center"
                className={Styles.cancelButton}
                onClick={() => cancelhandler()}
              >
                Cancel
              </Button>
              <Button
                type="submit"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
              >
                Save
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
        <CustomSidePopup
          open={showClientForm}
          title={'Add Client'}
          handleClose={handleClientFormClose}
          content={
            <CustomClientAdd
              isVissible={showClientForm}
              onAction={setShowClientForm}
            />
          }
        />
      </div>
    </div>
  );
};

export default ProjectGeneralDetails;
