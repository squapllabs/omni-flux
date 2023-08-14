import React, { useState, useRef } from 'react';
import Styles from '../../styles/projectForm.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import TextArea from '../ui/CustomTextArea';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import { getCreateValidateyup } from '../../helper/constants/project-constants';
import { useNavigate } from 'react-router';
import CustomSnackBar from '../ui/customSnackBar';
import { useGetAllClientDrop } from '../../hooks/client-hooks';
import { useGetAllUsersDrop } from '../../hooks/user-hooks';
import { useGetAllSiteDrop } from '../../hooks/site-hooks';
import siteService from '../../service/site-service';
import DeleteIcon from '../menu/icons/deleteIcon';
import AddIcon from '../menu/icons/addIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import CloseIcon from '../menu/icons/closeIcon';
import { createProject } from '../../hooks/project-hooks';
import { useGetMasterCurency } from '../../hooks/masertData-hook';
import userService from '../../service/user-service';

const ProjectForm = () => {
  const [message, setMessage] = useState('');
  const { mutate: createNewProjectData } = createProject();
  const [openSnack, setOpenSnack] = useState(false);
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const { data: getAllClientDatadrop = [] } = useGetAllClientDrop();
  const { data: getAllCurrencyDatadrop = [] } = useGetMasterCurency();
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const { data: getAllSite = [] } = useGetAllSiteDrop();
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [rows, setRows] = useState([
    {
      siteId: '',
      siteData: null,
      estimation: '',
      approver_id:''
    },
  ]);
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
    priority: '',
    estimated_budget: '',
    actual_budget: '',
    description: '',
    project_notes: '',
    site_configuration: [] as Array<{
      site_id: number;
      status: string;
      is_delete: string;
      estimation: number;
      approver_id:number;
    }>,
    project_documents: '',
    status: '',
  });
  const navigate = useNavigate();

  const getAllProjectPriorityType = [
    { label: 'High', value: 'High' },
    { label: 'Medium', value: 'Medium' },
    { label: 'Low', value: 'Low' },
  ];

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleDocuments = async (files: File[],code : string) => {
    try {
      const uploadPromises = files.map(async (file) => {
        const response = await userService.documentUpload(file,code);
        return response.data;
      });
      const uploadResponses = await Promise.all(uploadPromises);
      const modifiedArray = uploadResponses.flatMap((response) => response);
      const modifiedArrayWithDeleteFlag = modifiedArray.map((obj) => ({
        ...obj,
        is_delete: "N"
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
      const estimationValue = newSite[rowIndex]?.estimation || 0;
      newSite[rowIndex] = {
        site_id: Number(siteId),
        status: 'Not Started',
        is_delete: 'N',
        estimation: Number(estimationValue),
        approver_id:0
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
    setRows([...rows, { siteId: '', siteData: null, estimation: '',approver_id:'' }]);
  };

  const deleteRow = (index: any) => {
    const updatedRows = rows.filter((_, rowIndex) => rowIndex !== index);
    setRows(updatedRows);

    const newSite = [...formik.values.site_configuration];
    newSite.splice(index, 1);
    formik.setFieldValue('site_configuration', newSite);
  };

  const validationSchema = getCreateValidateyup(Yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    onSubmit: async (values) => {
      const s3UploadUrl = await handleDocuments(selectedFiles,values.code.toUpperCase());
      const Object: any = {
        project_name: values.project_name,
        code: values.code.toUpperCase(),
        user_id: Number(values.user_id),
        client_id: Number(values.client_id),
        date_started: values.date_started,
        date_ended: values.date_ended,
        priority: values.priority,
        estimated_budget: Number(values.estimated_budget),
        actual_budget: Number(values.actual_budget),
        description: values.description,
        project_notes: values.project_notes,
        site_configuration: values.site_configuration,
        project_documents: s3UploadUrl,
        status: 'Not Started',
      };
      createNewProjectData(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('Project created');
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
          console.log('data', file);
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
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.textContent}>
        <h3>Add - Project</h3>
        <span className={Styles.content}>Add your project</span>
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
                name="approver_id"
                onChange={formik.handleChange}
                mandatory={true}
                // value={formik.values.approver_id}
                defaultLabel="Select from options"
                // error={formik.touched.approver_id && formik.errors.approver_id}
              >
                {getAllCurrencyDatadrop.map((option: any) => (
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
                mandatory={true}
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
                    {/* <th className={Styles.tableHeading}>Approver</th> */}
                    <th className={Styles.tableHeading}>Action</th>
                  </tr>
                </thead>
                <tbody>
                  {rows.map((row, index) => (
                    <tr key={index}>
                      <td>{index + 1}</td>
                      <td>
                        <div
                          style={{
                            display: 'flex',
                            justifyContent: 'flex-start',
                            height: '50px',
                          }}
                        >
                          <Select
                            name="site_configuration"
                            onChange={(event) => handleSiteChange(event, index)}
                            value={row.siteId}
                            defaultLabel="Select from options"
                            error={
                              formik.touched.site_configuration &&
                              formik.errors.site_configuration
                            }
                          >
                            {getAllSite.map((option: any) => (
                              <option key={option.value} value={option.value}>
                                {option.label}
                              </option>
                            ))}
                          </Select>
                        </div>
                      </td>
                      <td>
                        <div
                          style={{
                            display: 'flex',
                            justifyContent: 'center',
                          }}
                        >
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
                        <div>
                          {row.siteId && (
                            <span
                              style={{
                                backgroundColor: 'lightgray',
                                // width: '70%',
                              }}
                            >
                              Not Started
                            </span>
                          )}
                        </div>
                      </td>
                      <td>
                        <div>
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
                              // error={
                              //   formik.touched.actual_budget &&
                              //   formik.errors.actual_budget
                              // }
                            />
                          ) : (
                            ''
                          )}
                        </div>
                      </td>
                      {/* <td>
                        <div>
                        {row.siteId ? (
                          <Select
                          name={`site_configuration[${index}].approver_id`}
                            onChange={formik.handleChange}
                            value={formik.values.client_id}
                            defaultLabel="Select from options"
                            error={
                              formik.touched.client_id &&
                              formik.errors.client_id
                            }
                          >
                            {getAllClientDatadrop.map((option: any) => (
                              <option key={option.value} value={option.value}>
                                {option.label}
                              </option>
                            ))}
                          </Select>
                            ) : 
                                ''
                              }
                        </div>
                      </td> */}

                      <td>
                        <div
                          style={{
                            display: 'flex',
                            justifyContent: 'center',
                          }}
                        >
                          {index === 0 ? (
                            row.siteId ? (
                              <DeleteIcon onClick={() => deleteRow(index)} />
                            ) : (
                              ''
                            )
                          ) : (
                            <DeleteIcon onClick={() => deleteRow(index)} />
                          )}
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
              <div
                style={{
                  display: 'flex',
                  justifyContent: 'flex-end',
                  padding: '10px',
                }}
              >
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
          <div style={{ padding: '10px 0px 0px 20px' }}>
            <div
              style={{
                width: '40%',
                border: '1px dashed #475467',
                borderRadius: '5px',
              }}
            >
              <div
                style={{
                  display: 'flex',
                  flexDirection: 'row',
                  justifyContent: 'space-between',
                  padding: '20px',
                }}
              >
                <div>
                  <UploadIcon />
                </div>
                <div
                  id="drop-area"
                  onDrop={(e) => handleDrop(e)}
                  onDragOver={(e) => e.preventDefault()}
                >
                  <h6>Select a file or drag and drop here</h6>
                  <span style={{ fontSize: '0.65rem' }}>
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
          <div style={{ padding: '0px 0px 0px 50px' }}>
            <span>
              <ol style={{ fontSize: '0.85rem' }}>
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
              <p style={{ color: 'red', fontSize: '0.75rem' }}>
                {fileSizeError}
              </p>
            </span>
          </div>
          <div className={Styles.submitButton}>
            <Button
              className={Styles.resetButton}
              type="submit"
              shape="rectangle"
              justify="center"
              onClick={() => navigate('/settings')}
            >
              Back
            </Button>
            <Button
              type="submit"
              color="primary"
              shape="rectangle"
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
    </div>
  );
};

export default ProjectForm;
