import React, { useState, useRef, useEffect } from 'react';
import Styles from '../../styles/projectForm.module.scss';
import { useFormik } from 'formik';
import * as yup from 'yup';
import Input from '../ui/Input';
import TextArea from '../ui/CustomTextArea';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
// import { getEditValidateyup } from '../../helper/constants/project-constants';
import { useNavigate } from 'react-router';
import CustomSnackBar from '../ui/customSnackBar';
import { useGetAllClientDrop } from '../../hooks/client-hooks';
import { useGetAllUsersDrop } from '../../hooks/user-hooks';
import { useGetAllSiteDrop } from '../../hooks/site-hooks';
import siteService from '../../service/site-service';
import AddIcon from '../menu/icons/addIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import { updateProject, getByProjectId } from '../../hooks/project-hooks';
import userService from '../../service/user-service';
import { useParams } from 'react-router-dom';
import { format } from 'date-fns';
import CloseIcon from '../menu/icons/closeIcon';
import MenuIcon from '../menu/icons/moreHorizontalIcon';
import CustomConfirm from '../ui/CustomConfirmDialogBox';
import BackArrow from '../menu/icons/backArrow';

const ProjectEdit = () => {
  const routeParams = useParams();
  const { data: getOneProjectData, isLoading } = getByProjectId(
    Number(routeParams?.id)
  );
  const [message, setMessage] = useState('');
  const { mutate: updateProjectData } = updateProject();
  const [openSnack, setOpenSnack] = useState(false);
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const { data: getAllClientDatadrop = [] } = useGetAllClientDrop();
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [existingFileName, setExistingFileName] = useState<string[]>([]);
  const [existingFileUrl, setExistingFileUrl] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const { data: getAllSite = [] } = useGetAllSiteDrop();
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [openConfirm, setOpenConfirm] = useState(false);
  const [rows, setRows] = useState([
    {
      siteId: '',
      siteData: null,
      estimation: '',
      approvar_id: '',
      status: '',
    },
  ]);
  const componentRef = useRef<HTMLDivElement | null>(null);
  const menuRef = useRef<Array<HTMLDivElement | null>>(
    new Array(rows.length).fill(null)
  );
  const [openDropdowns, setOpenDropdowns] = useState<number | null>(null);
  const [initialValues, setInitialValues] = useState({
    project_name: '',
    code: '',
    user_id: '',
    client_id: '',
    date_started: '',
    date_ended: '',
    priority: '',
    approvar_id: '',
    estimated_budget: '',
    actual_budget: '',
    description: '',
    project_notes: '',
    site_configuration: [] as Array<{
      site_id: number;
      status: string;
      is_delete: string;
      estimation: '';
      approvar_id: '';
    }>,
    project_documents: '',
    status: '',
    submitType: '',
  });
  const navigate = useNavigate();

  const getAllProjectPriorityType = [
    { label: 'High', value: 'High' },
    { label: 'Medium', value: 'Medium' },
    { label: 'Low', value: 'Low' },
  ];

  const getAllSiteStatus = [
    { label: 'Not Started', value: 'Not Started' },
    { label: 'Inprogress', value: 'Inprogress' },
    { label: 'On Hold', value: 'On Hold' },
    { label: 'Rejected', value: 'Rejected' },
    { label: 'Completed', value: 'Completed' },
  ];

  const toggleDropdown = (rowIndex: number) => {
    console.log('clicked', rowIndex);
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

  const handleSiteChange = async (event: any, rowIndex: any) => {
    const siteId = event.target.value;
    const updatedRows = rows.map((row, index) =>
      index === rowIndex ? { ...row, siteId } : row
    );
    setRows(updatedRows);
    if (siteId) {
      const siteData = await siteService.getOneSiteById(siteId);
      setRows((prevRows) => {
        const updatedRows = [...prevRows];
        updatedRows[rowIndex].siteData = siteData?.data;
        return updatedRows;
      });
      const newSite = [...formik.values.site_configuration];
      // const estimationValue = newSite[rowIndex]?.estimation || 0;
      // const approvar_idValue = newSite[rowIndex]?.approvar_id || 0;
      const statusValue = newSite[rowIndex]?.status || '';
      newSite[rowIndex] = {
        site_id: Number(siteId),
        status: statusValue,
        is_delete: 'N',
        estimation: '',
        approvar_id: '',
      };
      formik.setFieldValue('site_configuration', newSite);
    } else {
      setRows((prevRows) => {
        const updatedRows = [...prevRows];
        updatedRows[rowIndex].siteData = null;
        return updatedRows;
      });
      const newSite = [...formik.values.site_configuration];
      newSite.splice(rowIndex, 1);
      formik.setFieldValue('site_configuration', newSite);
    }
  };

  const addRow = () => {
    setRows([
      ...rows,
      {
        siteId: '',
        siteData: null,
        estimation: '',
        approvar_id: '',
        status: '',
      },
    ]);
  };

  useEffect(() => {
    const handleOutsideClick = (event: MouseEvent) => {
      console.log('Outside click triggered');
      console.log('openDropdowns:', openDropdowns);
      console.log('componentRef:', componentRef.current);
      console.log('menuRef:', menuRef.current);
      if (
        openDropdowns !== -1 &&
        componentRef.current &&
        !componentRef.current.contains(event.target as Node) &&
        menuRef.current.every(
          (dropdown) => !dropdown?.contains(event.target as Node)
        )
      ) {
        console.log('Closing dropdown due to outside click');
        setOpenDropdowns(null);
      }
    };
    document.addEventListener('click', handleOutsideClick);
    return () => {
      document.removeEventListener('click', handleOutsideClick);
    };
  }, [openDropdowns]);

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
      const siteConfigurationDatas = siteConfigurationData.map((site: any) => ({
        site_id: site.site_id,
        status: site.status,
        is_delete: 'N',
        project_site_id: site.project_site_id,
        estimation: site.estimation,
        approvar_id: site.approvar_id,
      }));
      setInitialValues({
        project_name: getOneProjectData?.project_name || '',
        code: getOneProjectData?.code || '',
        user_id: getOneProjectData?.user_id || '',
        client_id: getOneProjectData?.client_id || '',
        date_started: formattedDate || '',
        date_ended: formattedEndDate || '',
        priority: getOneProjectData?.priority || '',
        approvar_id: getOneProjectData?.approvar_id || '',
        estimated_budget: getOneProjectData?.estimated_budget || '',
        actual_budget: getOneProjectData?.actual_budget || '',
        description: getOneProjectData?.description || '',
        project_notes: getOneProjectData?.project_notes || '',
        site_configuration: siteConfigurationDatas,
        project_documents: {},
      });
      const siteConfigurationRows = siteConfigurationData.map(
        (config: any) => ({
          siteId: config.site_id.toString(),
          siteData: config.site_details,
          estimation: config.estimation,
          approvar_id: config.approvar_id.toString(),
          status: config.status,
        })
      );
      setRows(siteConfigurationRows);
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

  const validateSchema = yup.object().shape({
    project_name: yup.string().required('Project name is required'),
    user_id: yup.string().trim().required('Project manager is required'),
    approvar_id: yup.string().trim().required('Approver is required'),
    client_id: yup
      .string()
      .trim()
      .required('Project client/customer is required'),
    estimated_budget: yup
      .number()
      .min(1, 'Value must be greater than 0')
      .max(100000, 'Value must be less then 100000')
      .typeError('Only Number are allowed')
      .required('Estimated budget is required'),
    actual_budget: yup
      .number()
      .min(1, 'Value must be greater than 0')
      .max(100000, 'Value must be less then 100000')
      .typeError('Only Number are allowed'),
    priority: yup.string().trim().required('Priority is required'),
    date_started: yup.date().required('Project start date is required'),
    date_ended: yup
      .date()
      .required('Project end date is required')
      .min(
        yup.ref('date_started'),
        'Project end date cannot be earlier than start date'
      ),
    site_configuration: yup
      .array()
      .test(
        'unique-site-ids',
        'Site name repeated are not allowed',
        function (sites: any) {
          const siteIds = new Set();
          for (const site of sites) {
            if (siteIds.has(site.site_id)) {
              return false; // Duplicate site_id found
            }
            siteIds.add(site.site_id);
          }
          return true; // No duplicate site_id found
        }
      )
      .test('non-empty-array', 'Site is required', function (sites: any) {
        return sites.length > 0; // Check if array is not empty
      })
      .of(
        yup.object().shape({
          approvar_id: yup.string().trim().required('Approver is required'), // Validate approvar_id
          estimation: yup
            .number()
            .required('Estimation is required')
            .typeError('Only numbers are allowed')
            .test(
              'site-budget',
              'Site budget is greater than estimated budget',
              function () {
                const estimated_budget = Number(formik.values.estimated_budget);
                const site_configuration = formik.values.site_configuration;
                console.log('estimated_budget =>', estimated_budget);
                console.log('site_configuration', site_configuration);
                const totalEstimation = site_configuration.reduce(
                  (total: any, site: any) => total + site.estimation,
                  0
                );
                console.log('totalEstimation', totalEstimation);
                if (totalEstimation > estimated_budget) {
                  return false;
                }
                return true;
              }
            ),
        })
      ),
  });
  // const validationSchema = getEditValidateyup(Yup);
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
        priority: values.priority,
        approvar_id: Number(values.approvar_id),
        estimated_budget: Number(values.estimated_budget),
        actual_budget: Number(values.actual_budget),
        description: values.description,
        project_notes: values.project_notes,
        site_configuration: values.site_configuration,
        project_documents: newExistingFileUrl,
        status: statusData,
        project_id: Number(routeParams?.id),
      };
      console.log('finalObject===>', Object);
      updateProjectData(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('Project edited');
            setOpenSnack(true);
            setInterval(() => {
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
    // newFiles.splice(index, 1);
    newFileNames.splice(index, 1);
    setExistingFileUrl(newFiles);
    setExistingFileName(newFileNames);
  };

  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };

  const handleSampleClick = () => {
    console.log('sample demo click ');
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

  if (isLoading) {
    return <div>Loading...</div>;
  }
  return (
    <div ref={componentRef} className={Styles.container}>
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
              <Select
                label="Project Manager"
                name="user_id"
                mandatory={true}
                onChange={formik.handleChange}
                value={formik.values.user_id}
                defaultLabel="Select from options"
                error={formik.touched.user_id && formik.errors.user_id}
              >
                {getAllUsersDatadrop.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div style={{ width: '40%' }}>
              <Select
                label="Client / Customer"
                name="client_id"
                mandatory={true}
                onChange={formik.handleChange}
                value={formik.values.client_id}
                defaultLabel="Select from options"
                error={formik.touched.client_id && formik.errors.client_id}
              >
                {getAllClientDatadrop.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
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
                label="Priority"
                name="priority"
                mandatory={true}
                onChange={formik.handleChange}
                value={formik.values.priority}
                defaultLabel="Select from options"
                error={formik.touched.priority && formik.errors.priority}
              >
                {getAllProjectPriorityType.map((option: any) => (
                  <option key={option.value} value={option.value}>
                    {option.label}
                  </option>
                ))}
              </Select>
            </div>
            <div style={{ width: '40%' }}>
              <Select
                label="Approver"
                name="approvar_id"
                onChange={formik.handleChange}
                mandatory={true}
                value={formik.values.approvar_id}
                defaultLabel="Select from options"
                error={formik.touched.approvar_id && formik.errors.approvar_id}
              >
                {getAllUsersDatadrop.map((option: any) => (
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
          <div className={Styles.siteHeading}>
            <h4>Site Configuration</h4>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table>
                <thead>
                  <tr>
                    <th className={Styles.tableHeading}>S No</th>
                    <th className={Styles.tableHeading}>Site</th>
                    <th className={Styles.tableHeading}>Site Address</th>
                    <th className={Styles.tableHeading}>Status</th>
                    <th className={Styles.tableHeading}>Estimation</th>
                    <th className={Styles.tableHeading}>Approver</th>
                    {/* <th className={Styles.tableHeading}>Actions</th> */}
                  </tr>
                </thead>
                <tbody>
                  {rows.map((row, index) => (
                    <tr key={index}>
                      <td>{index + 1}</td>
                      <td>
                        <div className={Styles.selectedProjectName}>
                          {row.siteData &&
                          row.siteData.name &&
                          row.siteData.code ? (
                            <div>
                              <p>{row.siteData.name}</p>
                              <span className={Styles.projectCode}>
                                {row.siteData.code}
                              </span>
                            </div>
                          ) : (
                            <div className={Styles.siteField}>
                              <Select
                                name="site_configuration"
                                onChange={(event) =>
                                  handleSiteChange(event, index)
                                }
                                value={row.siteId}
                                defaultLabel="Select from options"
                                error={
                                  formik.touched.site_configuration &&
                                  formik.errors.site_configuration
                                }
                              >
                                {getAllSite.map((option: any) => (
                                  <option
                                    key={option.value}
                                    value={option.value}
                                  >
                                    {option.label}
                                  </option>
                                ))}
                              </Select>
                            </div>
                          )}
                        </div>
                      </td>
                      <td>
                        <div className={Styles.address}>
                          {row.siteData?.address && (
                            <div>
                              <p>
                                {row.siteData && row.siteData.address ? (
                                  <div>
                                    <p>
                                      {row.siteData.address.street}{' '}
                                      {row.siteData.address.city},{' '}
                                      {row.siteData.address.state},
                                    </p>
                                    <p>
                                      {row.siteData.address.pin_code},
                                      {row.siteData.address.country}
                                    </p>
                                  </div>
                                ) : null}
                              </p>
                            </div>
                          )}
                        </div>
                      </td>
                      <td>
                        {row.siteId ? (
                          <div className={Styles.siteEstimation}>
                            <Select
                              width="170px"
                              name={`site_configuration[${index}].status`}
                              onChange={formik.handleChange}
                              value={
                                formik.values.site_configuration[index]
                                  ?.status || ''
                              }
                              defaultLabel="Select from options"
                            >
                              {getAllSiteStatus.map((option: any) => (
                                <option key={option.value} value={option.value}>
                                  {option.label}
                                </option>
                              ))}
                            </Select>
                          </div>
                        ) : (
                          ''
                        )}
                      </td>
                      <td>
                        <div className={Styles.siteEstimation}>
                          {row.siteId ? (
                            <Input
                              placeholder="Enter estimation"
                              name={`site_configuration[${index}].estimation`}
                              onChange={(event) => {
                                const newValue = parseFloat(event.target.value);
                                formik.setFieldValue(
                                  `site_configuration[${index}].estimation`,
                                  newValue
                                );
                              }}
                              value={
                                formik.values.site_configuration[index]
                                  ?.estimation || ''
                              }
                              error={
                                formik.touched.site_configuration &&
                                formik.errors.site_configuration &&
                                formik.errors.site_configuration[index]
                                  ?.estimation
                              }
                            />
                          ) : (
                            ''
                          )}
                        </div>
                      </td>
                      <td>
                        {row.siteId ? (
                          <div className={Styles.siteEstimation}>
                            <Select
                              width="170px"
                              name={`site_configuration[${index}].approvar_id`}
                              onChange={(event) => {
                                const newValue = parseFloat(event.target.value);
                                formik.setFieldValue(
                                  `site_configuration[${index}].approvar_id`,
                                  newValue
                                );
                              }}
                              value={
                                formik.values.site_configuration[index]
                                  ?.approvar_id || ''
                              }
                              defaultLabel="Select from options"
                              error={
                                formik.touched.site_configuration &&
                                formik.errors.site_configuration &&
                                formik.errors.site_configuration[index]
                                  ?.approvar_id
                              }
                            >
                              {getAllUsersDatadrop.map((option: any) => (
                                <option key={option.value} value={option.value}>
                                  {option.label}
                                </option>
                              ))}
                            </Select>
                          </div>
                        ) : (
                          ''
                        )}
                      </td>
                      {/* <td>
                        <div ref={menuRef.current[index]}>
                          <MenuIcon onClick={() => toggleDropdown(index)} />
                          {openDropdowns === index && (
                            <ul className={Styles.menu}>
                              <li
                                className={Styles.menuItem}
                                onClick={() => handleSampleClick()}
                              >
                                <span>Add Bom</span>
                              </li>
                              <li
                                className={Styles.menuItem}
                                onClick={() => handleSampleClick()}
                              >
                                <span>Add Expenses</span>
                              </li>
                            </ul>
                          )}
                        </div>
                      </td> */}
                    </tr>
                  ))}
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
    </div>
  );
};

export default ProjectEdit;
