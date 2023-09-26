import React, { useState, useRef, useEffect, ChangeEvent } from 'react';
import Styles from '../../styles/projectForm.module.scss';
import { useFormik } from 'formik';
import * as yup from 'yup';
import Input from '../ui/Input';
import TextArea from '../ui/CustomTextArea';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import { useNavigate } from 'react-router';
import CustomSnackBar from '../ui/customSnackBar';
import { useGetAllClientDrop } from '../../hooks/client-hooks';
import { useGetAllUsersDrop, useGetAllUsers } from '../../hooks/user-hooks';
import { useGetAllSiteDrops } from '../../hooks/site-hooks';
import siteService from '../../service/site-service';
import AddIcon from '../menu/icons/addIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import {
  updateProject,
  getByProjectId,
  useGetMasterProjectParentType,
} from '../../hooks/project-hooks';
import userService from '../../service/user-service';
import { useParams } from 'react-router-dom';
import { format } from 'date-fns';
import CloseIcon from '../menu/icons/closeIcon';
import CustomConfirm from '../ui/CustomConfirmDialogBox';
import BackArrow from '../menu/icons/backArrow';
import CustomClientAdd from '../ui/CustomClientAdd';
import CustomSiteAdd from '../ui/CustomSiteAdd';
import MoreIcon from '../menu/icons/moreHorizontalIcon';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import CustomLoader from '../ui/customLoader';
const ProjectEdit = () => {
  const routeParams = useParams();
  const { data: getOneProjectData, isLoading } = getByProjectId(
    Number(routeParams?.id)
  );
  const isStatusInProgress = getOneProjectData?.status === 'Inprogress';

  const [message, setMessage] = useState('');
  const { mutate: updateProjectData } = updateProject();
  const [openSnack, setOpenSnack] = useState(false);
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const { data: getAllClientDatadrop = [] } = useGetAllClientDrop();
  const { data: getAllUsersSiteDatadrop = [] } = useGetAllUsers();
  const { data: getAllProjectTypeDatadrop = [] } =
    useGetMasterProjectParentType();
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [existingFileName, setExistingFileName] = useState<string[]>([]);
  const [existingFileUrl, setExistingFileUrl] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const { data: getAllSite = [] } = useGetAllSiteDrops();
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [siteConfigData, setSiteConfigData] = useState<any[]>([]);
  const [viewAddress, setViewAddress] = useState({});
  const [showClientForm, setShowClientForm] = useState(false);
  const [showSiteForm, setShowSiteForm] = useState(false);
  const [openDropdowns, setOpenDropdowns] = useState<number | null>(null);
  const menuRef = useRef<Array<React.RefObject<HTMLDivElement>>>(
    new Array(siteConfigData.length).fill(React.createRef())
  );
  const valueObject: any = {
    site_id: '',
    estimated_budget: '',
    actual_budget: '',
    approvar_id: '',
    status: '',
    is_delete: 'N',
    address: '',
  };
  const [value, setValue] = useState(valueObject);
  const [errors, setErrors] = useState('');
  let rowIndex = 0;
  const [openConfirm, setOpenConfirm] = useState(false);
  const [initialValues, setInitialValues] = useState({
    project_name: '',
    code: '',
    user_id: '',
    client_id: '',
    date_started: '',
    date_ended: '',
    project_type: '',
    approvar_id: '',
    project_estimated_budget: '',
    actual_budget: '',
    description: '',
    project_notes: '',
    site_configuration: '',
    project_documents: '',
    status: '',
    submitType: '',
  });
  const navigate = useNavigate();

  useEffect(() => {
    if (getOneProjectData) {
      const startDate = getOneProjectData?.date_started;
      const endDate = getOneProjectData?.date_ended;
      let formattedDate = '';
      let formattedEndDate = '';
      if (startDate) {
        const currentDate = new Date(startDate);
        formattedDate = format(currentDate, 'yyyy-MM-dd');
      }
      if (endDate) {
        const currentDate = new Date(endDate);
        formattedEndDate = format(currentDate, 'yyyy-MM-dd');
      }
      const siteConfigurationData = getOneProjectData.project_site;

      setInitialValues({
        project_name: getOneProjectData?.project_name || '',
        code: getOneProjectData?.code || '',
        user_id: getOneProjectData?.user_id || '',
        client_id: getOneProjectData?.client_id || '',
        date_started: formattedDate || '',
        date_ended: formattedEndDate || '',
        project_type: getOneProjectData?.project_type || '',
        approvar_id: getOneProjectData?.approvar_id || '',
        project_estimated_budget: getOneProjectData?.estimated_budget || '',
        actual_budget: getOneProjectData?.actual_budget || '',
        description: getOneProjectData?.description || '',
        project_notes: getOneProjectData?.project_notes || '',
        project_documents: {},
      });
      const siteConfigurationRows = siteConfigurationData.map(
        (config: any) => ({
          site_id: config.site_id,
          siteData: config.site_details,
          estimated_budget: config.estimated_budget,
          actual_budget: config.actual_budget,
          approvar_id: config.approvar_id,
          status: config.status,
          project_site_id: config.project_site_id,
        })
      );
      setSiteConfigData(siteConfigurationRows);
      const existingDocumentsS3Url = getOneProjectData.project_documents;
      setExistingFileUrl(existingDocumentsS3Url);
      const existingFileNames = existingDocumentsS3Url.map((document: any) => {
        const pathParts = document.path.split('/');
        const fileName = pathParts[pathParts.length - 1];
        const originalFileNameMatches = fileName.match(/-.*-(.*\.\w+)/);
        if (originalFileNameMatches) {
          return originalFileNameMatches[1];
        }
        return fileName;
      });
      setExistingFileName(existingFileNames);
    }
  }, [getOneProjectData]);

  const getAllSiteStatus = [
    { name: 'Not Started', value: 'Not Started' },
    { name: 'Inprogress', value: 'Inprogress' },
    { name: 'On Hold', value: 'On Hold' },
    { name: 'Rejected', value: 'Rejected' },
    { name: 'Completed', value: 'Completed' },
  ];

  const handleDropdown = (rowIndex: number) => {
    if (openDropdowns === rowIndex) {
      setOpenDropdowns(-1);
    } else {
      setOpenDropdowns(rowIndex);
    }
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleDocuments = async (files: File[]) => {
    try {
      const uploadPromises = files.map(async (file) => {
        const response = await userService.user_profile_upload(file);
        return response.data;
      });
      const uploadResponses = await Promise.all(uploadPromises);
      const modifiedArray = uploadResponses.flatMap((response) => response);
      const modifiedArrayWithDeleteFlag = modifiedArray.map((obj) => ({
        ...obj,
        is_delete: 'N',
      }));
      return modifiedArrayWithDeleteFlag;
    } catch (error) {
      console.log('Error in occur project document upload:', error);
    }
  };

  const handleChangeItems = async (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    if (event.target.name === 'status') {
      setValue({
        ...value,
        [event.target.name]: event.target.value,
      });
    } else {
      setValue({
        ...value,
        [event.target.name]: Number(event.target.value),
      });
    }
    if (event.target.name === 'site_id') {
      const siteId = event.target.value;
      const siteData = await siteService.getOneSiteById(siteId);
      setViewAddress(siteData?.data);
    }
  };

  const addressSet = async (value: any) => {
    const siteId = value;
    const siteData = await siteService.getOneSiteById(siteId);
    setViewAddress(siteData?.data);
  };

  const handleChangeExistItems = (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>,
    index: number
  ) => {
    let tempObj = {};
    tempObj = {
      ...siteConfigData[index],
      [event.target.name]:
        event.target.name === 'status'
          ? event.target.value
          : Number(event.target.value),
    };
    const tempArray = [...siteConfigData];
    tempArray[index] = tempObj;
    setSiteConfigData(tempArray);
  };

  const addRow = async () => {
    setErrors({});
    const schema = yup.object().shape({
      site_id: yup
        .string()
        .trim()
        .required('Site is required')
        .test(
          'unique-site-ids',
          'Site name repeated are not allowed',
          function (sites: any) {
            const isSiteIdUnique = siteConfigData.every(
              (siteData) => siteData.site_id !== parseInt(sites, 10)
            );
            return isSiteIdUnique;
          }
        ),
      approvar_id: yup.string().trim().required('Approver is required'),
      actual_budget: yup
        .string()
        .matches(/^[0-9]*$/, 'Only numbers are allowed'),
      estimated_budget: yup
        .string()
        .matches(/^[0-9]*$/, 'Only numbers are allowed')
        .required('Budget is required')
        .typeError('Only numbesqswqsrs are allowed')
        .test(
          'site-budget',
          'Site budget is greater than estimated budget',
          function (budget: any) {
            const estimated_budget = formik.values.project_estimated_budget;
            const site_configuration = siteConfigData;
            if (site_configuration.length === 0) {
              if (Number(budget) > Number(estimated_budget)) {
                return false;
              }
              return true;
            } else {
              const totalEstimation = site_configuration.reduce(
                (total: any, site: any) =>
                  total + Number(site.estimated_budget),
                0
              );
              const finalTotal = totalEstimation + Number(budget);
              if (finalTotal > estimated_budget) {
                return false;
              }
              return true;
            }
          }
        ),
    });
    await schema
      .validate(value, { abortEarly: false })
      .then(async () => {
        const siteData = await siteService.getOneSiteById(value.site_id);
        value['address'] = siteData?.data?.address;
        const updatedObject = {
          ...value,
          site_id: Number(value.site_id),
          estimated_budget: Number(value.estimated_budget),
          approvar_id: Number(value.approvar_id),
          actual_budget: value.actual_budget ? Number(value.actual_budget) : 0,
        };

        setSiteConfigData([...siteConfigData, updatedObject]);
        setValue({
          site_id: '',
          estimated_budget: '',
          approvar_id: '',
          status: 'Not Started',
          is_delete: 'N',
        });
      })
      .catch((e) => {
        const errorObj: any = {};
        e.inner.map((errors: any) => {
          return (errorObj[errors.path] = errors.message);
        });
        setErrors({
          ...errorObj,
        });
      });
  };

  const validateSchema = yup.object().shape({
    project_name: yup.string().required('Project name is required'),
    user_id: yup.string().trim().required('Project manager is required'),
    approvar_id: yup.string().trim().required('Approver is required'),
    client_id: yup
      .string()
      .trim()
      .required('Project client/customer is required'),
    project_estimated_budget: yup
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
    project_type: yup.string().trim().required('Project Type is required'),
    date_started: yup.date().required('Project start date is required'),
    date_ended: yup
      .date()
      .required('Project end date is required')
      .min(
        yup.ref('date_started'),
        'Project end date cannot be earlier than start date'
      ),
  });
  const formik = useFormik({
    initialValues,
    validationSchema: validateSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      let newExistingFileUrl = [...existingFileUrl];
      if (selectedFiles.length > 0) {
        const s3UploadUrl = await handleDocuments(selectedFiles);
        newExistingFileUrl = newExistingFileUrl.concat(s3UploadUrl);
      }
      const statusData = values.submitType === 'Draft' ? 'Draft' : 'Inprogress';
      const Object: any = {
        project_name: values.project_name,
        code: values.code.toUpperCase(),
        user_id: Number(values.user_id),
        client_id: Number(values.client_id),
        date_started: values.date_started,
        date_ended: values.date_ended,
        project_type: values.project_type,
        approvar_id: Number(values.approvar_id),
        estimated_budget: Number(values.project_estimated_budget),
        actual_budget: Number(values.actual_budget),
        description: values.description,
        project_notes: values.project_notes,
        site_configuration: siteConfigData,
        project_documents: newExistingFileUrl,
        status: statusData,
        project_id: Number(routeParams?.id),
      };
      updateProjectData(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('Project edited');

            setOpenSnack(true);
            setTimeout(() => {
              navigate('/settings');
            }, 1000);
          }
        },
      });
    },
  });

  const handleDrop = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    e.stopPropagation();
    const files = e.dataTransfer.files;
    const fileList = Array.from(files);
    const oversizedFiles = fileList.filter(
      (file) => file.size > 10 * 1024 * 1024
    );
    if (oversizedFiles.length > 0) {
      const oversizedFileNames = oversizedFiles
        .map((file) => file.name)
        .join(', ');
      const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
      setFileSizeError(errorMessage);
    } else {
      const selectedFilesArray: File[] = [];
      const selectedFileNamesArray: string[] = [];

      fileList.forEach((file) => {
        selectedFilesArray.push(file);
        const originalFileNameMatch = file.name.match(/-(\d+-\d+-.*)$/);
        const originalFileName = originalFileNameMatch
          ? originalFileNameMatch[1]
          : file.name;
        selectedFileNamesArray.push(originalFileName);
      });
      setSelectedFiles(selectedFilesArray);
      setSelectedFileName(selectedFileNamesArray);
      setFileSizeError('');
    }
  };

  const handleFileSelect = (e: any) => {
    const files = e.target.files;
    if (files.length > 0) {
      const fileList: File[] = Array.from(files);
      const oversizedFiles = fileList.filter(
        (file) => file.size > 10 * 1024 * 1024
      );
      if (oversizedFiles.length > 0) {
        const oversizedFileNames = oversizedFiles
          .map((file) => file.name)
          .join(', ');
        const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
        setFileSizeError(errorMessage);
      } else {
        const selectedFilesArray: File[] = [];
        const selectedFileNamesArray: string[] = [];
        fileList.forEach((file) => {
          selectedFilesArray.push(file);
          selectedFileNamesArray.push(file.name);
        });
        setSelectedFiles(selectedFilesArray);
        setSelectedFileName(selectedFileNamesArray);
        setFileSizeError('');
      }
    }
  };
  const deleteFile = (index: number) => {
    const newFiles = [...selectedFiles];
    const newFileNames = [...selectedFileName];
    newFiles.splice(index, 1);
    newFileNames.splice(index, 1);
    setSelectedFiles(newFiles);
    setSelectedFileName(newFileNames);
  };

  const deleteExistFile = (index: number) => {
    const newFiles = [...existingFileUrl];
    newFiles[index] = { ...newFiles[index], is_delete: 'Y' };
    const newFileNames = [...existingFileName];
    newFileNames.splice(index, 1);
    setExistingFileUrl(newFiles);
    setExistingFileName(newFileNames);
  };

  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };

  const drafthandler = () => {
    formik.setFieldValue('submitType', 'Draft');
    formik.submitForm();
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

  const handelOpenSiteForm = () => {
    setShowSiteForm(true);
  };

  const handleExpenses = (data: any) => {
    const siteId = data.siteData?.site_contractor_id;
    const projectSiteId = Number(routeParams?.id);
    navigate(`/expenses/${projectSiteId}/${siteId}`);
  };

  const handleBom = () => {
    const projectSiteId = Number(routeParams?.id);
    navigate(`/bomlist/${projectSiteId}`);
  };

  return (
    <div className={Styles.container}>
      <CustomLoader loading={isLoading} size={48} color="#333C44">
        <div className={Styles.containerMain}>
          <div className={Styles.textContent}>
            <h3>Edit - Project</h3>
            <span className={Styles.content}>Edit your project</span>
          </div>
          <div className={Styles.backButton}>
            <Button
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
              icon={<BackArrow />}
              onClick={() => navigate('/settings')}
            >
              Back
            </Button>
          </div>
        </div>
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
                  disabled
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
                  disabled
                  optionList={getAllUsersDatadrop}
                />
              </div>
              <div style={{ width: '40%' }}>
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
                  disabled
                  optionList={getAllClientDatadrop}
                />
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
                {/* <AutoCompleteSelect
                name="project_type"
                label="Project Type"
                defaultLabel="Select from options"
                mandatory={true}
                value={formik.values.project_type}
                onChange={formik.handleChange}
                error={
                  formik.touched.project_type && formik.errors.project_type
                }
                onSelect={(label) => {
                  formik.setFieldValue('project_type', label);
                }}
                optionList={getAllProjectTypeDatadrop}
                // disabled
              /> */}
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
                  optionList={getAllUsersDatadrop}
                />
              </div>
            </div>
            <div className={Styles.inputFields}>
              <div style={{ width: '40%' }}>
                <Input
                  label="Estimated Budget"
                  placeholder="Enter rate"
                  name="project_estimated_budget"
                  mandatory={true}
                  onChange={formik.handleChange}
                  value={formik.values.project_estimated_budget}
                  error={
                    formik.touched.project_estimated_budget &&
                    formik.errors.project_estimated_budget
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
            <div className={Styles.siteHeading}>
              <h4>Site Configuration</h4>
            </div>
            <div className={Styles.tableContainer}>
              <div>
                <table>
                  <thead>
                    <tr>
                      <th className={Styles.tableHeading}>S No</th>
                      <th className={Styles.tableHeadingSite}>Site</th>
                      <th className={Styles.tableHeading}>Site Address</th>
                      <th className={Styles.tableHeading}>Status</th>
                      <th className={Styles.tableHeading}>Estimate Budget</th>
                      <th className={Styles.tableHeading}>Actual Budget</th>
                      <th className={Styles.tableHeading}>Approver</th>
                      <th className={Styles.tableHeading}>Actions</th>
                    </tr>
                  </thead>
                  <tbody>
                    {siteConfigData.map((row, index) => {
                      rowIndex = rowIndex + 1;
                      return (
                        <tr key={index}>
                          <td>{rowIndex}</td>
                          <td>
                            <div className={Styles.selectedProjectName}>
                              <div className={Styles.siteField}>
                                <AutoCompleteSelect
                                  width="200px"
                                  name="site_id"
                                  defaultLabel="Select Site"
                                  placeholder="Select from options"
                                  value={row?.site_id}
                                  onSelect={(value) => {
                                    setValue({
                                      ...siteConfigData,
                                      ['site_id']: Number(value),
                                    });
                                  }}
                                  optionList={getAllSite}
                                />
                              </div>
                            </div>
                          </td>
                          <td>
                            <div>
                              <span>
                                {row.siteData ? (
                                  <>
                                    {row.siteData.address.street}{' '}
                                    {row.siteData.address.city},{' '}
                                    {row.siteData.address.state},
                                  </>
                                ) : (
                                  <>
                                    {row.address.street} {row.address.city},{' '}
                                    {row.address.state},
                                  </>
                                )}
                              </span>
                              <span>
                                {row.siteData ? (
                                  <>
                                    {row.siteData.address.pin_code},{' '}
                                    {row.siteData.address.country}
                                  </>
                                ) : (
                                  <>
                                    {row.address.pin_code},{' '}
                                    {row.address.country}
                                  </>
                                )}
                              </span>
                            </div>
                          </td>
                          <td>
                            <div className={Styles.siteEstimation}>
                              <Select
                                width="140px"
                                name="status"
                                onChange={(e) =>
                                  handleChangeExistItems(e, index)
                                }
                                value={row.status}
                                defaultLabel="Select from options"
                              >
                                {getAllSiteStatus.map((option: any) => (
                                  <option
                                    key={option.value}
                                    value={`${option.value}`}
                                  >
                                    {option.name}
                                  </option>
                                ))}
                              </Select>
                            </div>
                          </td>
                          <td>
                            <div className={Styles.siteEstimation}>
                              <Input
                                width="140px"
                                placeholder="Enter budget"
                                name="estimated_budget"
                                onChange={(e) =>
                                  handleChangeExistItems(e, index)
                                }
                                value={row?.estimated_budget}
                                // error={errors?.estimated_budget}
                              />
                            </div>
                          </td>
                          <td>
                            <div className={Styles.siteEstimation}>
                              <Input
                                width="140px"
                                placeholder="Enter budget"
                                name="actual_budget"
                                onChange={(e) =>
                                  handleChangeExistItems(e, index)
                                }
                                value={row.actual_budget}
                              />
                            </div>
                          </td>

                          <td>
                            <div className={Styles.siteEstimation}>
                              <AutoCompleteSelect
                                name="approvar_id"
                                value={row?.approvar_id}
                                placeholder="Select from options"
                                defaultLabel="Select Approver"
                                onSelect={(value) => {
                                  setValue({
                                    ...siteConfigData,
                                    ['approvar_id']: Number(value),
                                  });
                                }}
                                onChange={(e) =>
                                  handleChangeExistItems(e, index)
                                }
                                optionList={getAllUsersDatadrop}
                              />
                            </div>
                          </td>
                          <td>
                            <div ref={menuRef.current[index]}>
                              <MoreIcon onClick={() => handleDropdown(index)} />
                              {openDropdowns === index && (
                                <ul className={Styles.menu}>
                                  <li
                                    className={Styles.menuItem}
                                    onClick={() => handleBom()}
                                  >
                                    <span>Add Bom</span>
                                  </li>
                                  <li
                                    className={Styles.menuItem}
                                    onClick={() => handleExpenses(row)}
                                  >
                                    <span>Add Expenses</span>
                                  </li>
                                </ul>
                              )}
                            </div>
                          </td>
                        </tr>
                      );
                    })}
                    <tr>
                      <td>{rowIndex + 1}</td>
                      <td>
                        <div className={Styles.selectedProjectName}>
                          <div className={Styles.siteField}>
                            <AutoCompleteSelect
                              width="200px"
                              name="site_id"
                              defaultLabel="Select Site"
                              placeholder="Select from options"
                              value={value.site_id}
                              onSelect={(datas) => {
                                setValue((prevValue: any) => {
                                  const updatedValue = {
                                    ...prevValue,
                                    site_id: datas,
                                  };
                                  addressSet(updatedValue.site_id);
                                  return updatedValue;
                                });
                              }}
                              error={errors?.site_id}
                              optionList={getAllSite}
                            />
                          </div>
                          <div
                            className={Styles.instantAdd}
                            onClick={handelOpenSiteForm}
                          >
                            <AddIcon
                              style={{ height: '15px', width: '15px' }}
                            />
                            <h4 className={Styles.addtext}> Add Site</h4>
                          </div>
                        </div>
                      </td>
                      <td>
                        {' '}
                        {value.site_id && (
                          <div>
                            <span>
                              {viewAddress.address?.street}{' '}
                              {viewAddress.address?.city},{' '}
                              {viewAddress.address?.state},
                            </span>
                            <span>
                              {viewAddress.address?.pin_code},
                              {viewAddress.address?.country}
                            </span>
                          </div>
                        )}
                      </td>
                      <td>
                        {value.site_id ? (
                          <div className={Styles.siteEstimation}>
                            <Select
                              width="140px"
                              name="status"
                              onChange={handleChangeItems}
                              value={value.status}
                              defaultLabel="Select Status"
                            >
                              {getAllSiteStatus.map((option: any) => (
                                <option
                                  key={option.value}
                                  value={`${option.value}`}
                                >
                                  {option.name}
                                </option>
                              ))}
                            </Select>
                          </div>
                        ) : (
                          ''
                        )}
                      </td>
                      <td>
                        {value.site_id ? (
                          <div className={Styles.siteEstimation}>
                            <Input
                              width="140px"
                              placeholder="Enter budget"
                              name="estimated_budget"
                              onChange={handleChangeItems}
                              value={value.estimated_budget}
                              error={errors?.estimated_budget}
                            />
                          </div>
                        ) : (
                          ''
                        )}
                      </td>
                      <td>
                        {value.site_id ? (
                          <div className={Styles.siteEstimation}>
                            <Input
                              width="140px"
                              placeholder="Enter budget"
                              name="actual_budget"
                              onChange={handleChangeItems}
                              value={value.actual_budget}
                              error={errors?.actual_budget}
                            />
                          </div>
                        ) : (
                          ''
                        )}
                      </td>
                      <td>
                        {value.site_id ? (
                          <div className={Styles.siteEstimation}>
                            <AutoCompleteSelect
                              name="approvar_id"
                              defaultLabel="Select Approver"
                              placeholder="Select from options"
                              value={value.approvar_id}
                              onSelect={(datas) => {
                                setValue({ ...value, ['approvar_id']: datas });
                              }}
                              optionList={getAllUsersDatadrop}
                              error={errors?.approvar_id}
                            />
                          </div>
                        ) : (
                          ''
                        )}
                      </td>
                      <td></td>
                    </tr>
                  </tbody>
                </table>
                <div className={Styles.buttonContent}>
                  <Button
                    type="button"
                    color="primary"
                    shape="rectangle"
                    size="small"
                    justify="center"
                    icon={<AddIcon />}
                    onClick={addRow}
                  >
                    Add
                  </Button>
                </div>
              </div>
            </div>
            <div className={Styles.siteHeading}>
              <h4>Project Documents</h4>
            </div>
            <div className={Styles.documentContainer}>
              <div className={Styles.documentOuterLayer}>
                <div className={Styles.documentContent}>
                  <div>
                    <UploadIcon />
                  </div>
                  <div
                    id="drop-area"
                    onDrop={(e) => handleDrop(e)}
                    onDragOver={(e) => e.preventDefault()}
                  >
                    <h6>Select a file or drag and drop here</h6>
                    <span className={Styles.documentSpan}>
                      JPG,PNG or PDF, file size no more than 10MB
                    </span>
                  </div>
                  <input
                    ref={fileInputRef}
                    id="upload-photo"
                    name="upload_photo"
                    type="file"
                    style={{ display: 'none' }}
                    onChange={handleFileSelect}
                    multiple
                  />
                  <Button
                    onClick={onButtonClick}
                    type="button"
                    shape="rectangle"
                    size="small"
                  >
                    Add Files
                  </Button>
                </div>
              </div>
            </div>
            <div className={Styles.viewFiles}>
              <span>
                <ol className={Styles.listStyles}>
                  {existingFileName?.map((a: any, index: number) => (
                    <li key={index}>
                      {a}{' '}
                      <CloseIcon
                        width={5}
                        height={10}
                        onClick={() => deleteExistFile(index)}
                      />
                    </li>
                  ))}

                  {selectedFileName.map((fileName, index) => (
                    <li key={index}>
                      {fileName} {'    '}
                      <CloseIcon
                        width={5}
                        height={10}
                        onClick={() => deleteFile(index)}
                      />
                    </li>
                  ))}
                </ol>
              </span>
              <span>
                {' '}
                <p className={Styles.errorStyles}>{fileSizeError}</p>
              </span>
            </div>
            <div className={Styles.submitButton}>
              <Button
                className={Styles.resetButton}
                type="button"
                shape="rectangle"
                justify="center"
                onClick={() => drafthandler()}
                disabled={isStatusInProgress}
              >
                Draft
              </Button>
              <Button
                type="button"
                color="primary"
                shape="rectangle"
                justify="center"
                onClick={() => submitHandler()}
              >
                Submit
              </Button>
            </div>
          </div>
        </form>
        <div>
          <CustomClientAdd
            isVissible={showClientForm}
            onAction={setShowClientForm}
          />
          <CustomSiteAdd
            isVissiblesite={showSiteForm}
            onActionsite={setShowSiteForm}
          />
        </div>
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomConfirm
          open={openConfirm}
          title="Confirm Submit"
          contentLine1="If you confirmed this project it will move to the review process"
          handleClose={handleCloseConfirm}
          handleConfirm={handleConfirmForm}
        />
      </CustomLoader>
    </div>
  );
};

export default ProjectEdit;
