import React, { useRef, useState, useEffect } from 'react';
import Styles from '../../../styles/project.module.scss';
import CloseIcon from '../../menu/icons/closeIcon';
import UploadIcon from '../../menu/icons/cloudUpload';
import AddIcon from '../../menu/icons/addIcon';
import Button from '../../ui/Button';
import userService from '../../../service/user-service';
import { useNavigate, useParams } from 'react-router-dom';
import projectService from '../../../service/project-service';
import { useUpdateProject } from '../../../hooks/project-hooks';
import CustomSnackBar from '../../ui/customSnackBar';
import DeleteIcon from '../../menu/icons/newDeleteIcon';
import ViewIcon from '../../menu/icons/newViewIcon';
import DocumentIcon from '../../menu/icons/documentIcon';
import Popup from '../../ui/CustomPdfPopup';
import ProjectDocumentView from './projectDocumentView';
import FileUploadComponent from '../../ui/fileUploadComponent';

const ProjectDocument: React.FC = (props: any) => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowindex = 0;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [viewDocs, setViewDocs] = useState();
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [reload, setReload] = useState(false);
  const [openPdfpopup, setOpenPdfpopup] = useState(false);
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [projectDocs, setProjectDocs] = useState<any>([]);
  const [projectData, setProjectData] = useState<any>({});
  const [dataCount, setDataCount] = useState(0);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const { mutate: updateProjectData } = useUpdateProject();
  const [previewUrls, setPreviewUrls] = useState<any[]>([]);
  const [selectedFilesInChild, setSelectedFilesInChild] = useState<File[]>([]);
  const [refreshView, setRefreshView] = useState(false);
  /* Function to drag and drop a file */
  const handleDrop = async (e: React.DragEvent<HTMLDivElement>) => {
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

  /* Function to view a file */
  const viewDocument = (value: any) => {
    setOpenPdfpopup(true);
    setViewDocs(value);
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

  const handleSelectedFilesChange = (files: File[]) => {
    setSelectedFilesInChild(files);
  };

  useEffect(() => {
    const fetchData = async () => {
      const getData = await projectService.getOneProjectById(
        Number(routeParams?.id)
      );
      setProjectData(getData?.data);
      setProjectDocs(getData?.data?.project_documents);
      setDataCount(getData?.data?.project_documents.length);
    };
    if (routeParams?.id !== undefined) fetchData();
  }, [reload]);
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
    if (selectedFilesInChild?.length > 0 || projectDocs[0]?.is_delete) {
      const s3UploadUrl: any = await handleDocuments(
        selectedFilesInChild,
        projectData.code.toUpperCase()
      );
      const arr: any = [...projectDocs, ...s3UploadUrl];
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
            setPreviewUrls([]);
            setRefreshView(true);
            setReload(true);
            setTimeout(() => {
              navigate(`/project-edit/${data?.data?.project?.project_id}`);
              props.setLoader(props.loader);
              props.setActiveButton('PGS');
            }, 2000);
          }
        },
      });
    } else {
      setMessage('Please Choose Document');
      setOpenSnack(true);
    }
  };

  return (
    <div className={Styles.container_document}>
      <div className={Styles.topHeading}>
        <div className={Styles.heading}>
          <div className={Styles.subHeading}>
            <DocumentIcon width={30} height={30} />
            <h3>DOCUMENTS</h3>
          </div>
        </div>
      </div>

      <div>
        <FileUploadComponent
          enableDrop={true}
          handleDrop={handleDrop}
          refreshView={refreshView}
          setRefreshView={setRefreshView}
          viewDocument={viewDocument}
          onSelectedFilesChange={handleSelectedFilesChange}
        />
      </div>
      <div>
        <Button
          onClick={() => {
            handleSubmit();
          }}
          type="button"
          shape="rectangle"
          size="small"
          color="primary"
        >
          Save
        </Button>
      </div>
      <div className={Styles.tableDiv}>
        <table className={Styles.scrollable_table_documents}>
          <thead>
            <tr>
              <th>#</th>
              <th>Documents</th>
              <th>Action</th>
            </tr>
          </thead>
          <tbody>
            {projectDocs?.length > 0 ? (
              projectDocs.map((files: any, index: any) => {
                if (files?.is_delete === 'N') {
                  rowindex = rowindex + 1;
                  return (
                    <tr>
                      <td>{rowindex}</td>
                      <td>
                        <a href={files.path}>Document {rowindex}</a>
                      </td>
                      <td>
                        <div className={Styles.icons}>
                          {' '}
                          <ViewIcon
                            onClick={() => {
                              viewDocument(files?.path);
                            }}
                          />
                          <DeleteIcon
                            width={20}
                            height={15}
                            onClick={() => deleteFileinList(files.path)}
                          />
                        </div>
                      </td>
                    </tr>
                  );
                }
              })
            ) : (
              <tr>
                <td colSpan="3" style={{ textAlign: 'center' }}>
                  No documents found
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>

      {/* </div> */}
      <Popup
        // title="Pdf Viewer"
        openPopup={openPdfpopup}
        setOpenPopup={setOpenPdfpopup}
        content={
          <ProjectDocumentView
            openPopup={openPdfpopup}
            setOpenPopup={setOpenPdfpopup}
            viewDocs={viewDocs}
          />
        }
      />

      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="error"
      />
    </div>
  );
};

export default ProjectDocument;
