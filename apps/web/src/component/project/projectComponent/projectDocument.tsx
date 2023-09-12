import React, { useRef, useState, useEffect } from 'react';
import Styles from '../../../styles/project.module.scss';
import CloseIcon from '../../menu/icons/closeIcon';
import UploadIcon from '../../menu/icons/cloudUpload';
import Button from '../../ui/Button';
import userService from '../../../service/user-service';
import { useNavigate, useParams } from 'react-router-dom';
import projectService from '../../../service/project-service';
import { updateProject } from '../../../hooks/project-hooks';
import CustomSnackBar from '../../ui/customSnackBar';
import DeleteIcon from '../../menu/icons/deleteIcon';

const ProjectDocument: React.FC = (props: any) => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowindex = 0;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [projectDocs, setProjectDocs] = useState<any>([]);
  const [projectData, setProjectData] = useState<any>({});
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const { mutate: updateProjectData } = updateProject();
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
  const handleSnackBarClose = () => {
    setOpenSnack(false);
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
  const deleteFileinList = (data: any) => {
    const objectIndex = projectDocs.findIndex((obj: any) => obj.path === data);
    projectDocs[objectIndex] = {
      ...projectDocs[objectIndex],
      is_delete: 'Y',
    };
    setProjectDocs([...projectDocs]);
    rowindex = rowindex - 1;
  };
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };
  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectService.getOneProjectById(
        Number(routeParams?.id)
      );
      setProjectData(getData?.data);
      setProjectDocs(getData?.data?.project_documents);
    };
    if (routeParams?.id != undefined) fetchData();
  }, []);
  const handleDocuments = async (files: File[], code: string) => {
    try {
      const uploadPromises = files.map(async (file) => {
        const response = await userService.documentUpload(file, code);
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
  const handleSubmit = async () => {
    const s3UploadUrl: any = await handleDocuments(
      selectedFiles,
      projectData.code.toUpperCase()
    );
    let arr: any = [...projectDocs, ...s3UploadUrl];

    const obj: any = {
      ...projectData,
      site_configuration: projectData?.project_site,
      bom_configuration: projectData?.bom_configuration,
      project_documents: arr,
    };
    updateProjectData(obj, {
      onSuccess: (data, variables, context) => {
        if (data?.status === true) {
          setMessage('Document Uploaded');
          setOpenSnack(true);
          props.setLoader(!props.loader);
          setTimeout(() => {
            navigate(`/project-edit/${data?.data?.project?.project_id}`);
            props.setLoader(props.loader);
            props.setActiveButton('PGS');
          }, 2000);
        }
      },
    });
  };
  return (
    <div>
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
        <div>
          <Button
            onClick={(e) => {
              handleSubmit(e);
            }}
            type="button"
            shape="rectangle"
            size="small"
            color="primary"
          >
            Save
          </Button>
        </div>
      </div>

      <div className={Styles.viewFiles}>
        <span>
          <ol className={Styles.listStyles}>
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
      <div
        style={{ width: '40%', display: 'flex', justifyContent: 'flex-start' }}
      >
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <tr>
              <th>SI No</th>
              <th>Documents</th>
              <th>Action</th>
            </tr>
            <tbody>
              {projectDocs?.map((files: any, index: any) => {
                if (files?.is_delete === 'N') {
                  rowindex = rowindex + 1;
                  return (
                    <tr>
                      <td>{rowindex}</td>
                      <td>
                        <a href={files.path}>Document {rowindex}</a>
                      </td>
                      <td>
                        {' '}
                        <DeleteIcon
                          width={20}
                          height={15}
                          onClick={() => deleteFileinList(files.path)}
                        />
                      </td>
                    </tr>
                  );
                }
              })}
            </tbody>
          </table>
        </div>
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

export default ProjectDocument;
