import React, { useRef, useState, useEffect } from 'react';
import Styles from '../../../styles/project.module.scss';
import CloseIcon from '../../menu/icons/closeIcon';
import UploadIcon from '../../menu/icons/cloudUpload';
import AddIcon from '../../menu/icons/documentAddIcon';
import Button from '../../ui/Button';
import userService from '../../../service/user-service';
import { useNavigate, useParams } from 'react-router-dom';
import projectService from '../../../service/project-service';
import { updateProject } from '../../../hooks/project-hooks';
import CustomSnackBar from '../../ui/customSnackBar';
import DeleteIcon from '../../menu/icons/newDeleteIcon';
import ViewIcon from '../../menu/icons/newViewIcon';
import DocumentIcon from '../../menu/icons/documentIcon';
import Popup from '../../ui/CustomPdfPopup';
import ProjectDocumentView from './projectDocumentView';

const ProjectDocument: React.FC = (props: any) => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowindex = 0;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [viewDocs, setViewDocs] = useState();
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [reload, setReload] = useState(false)
  const [openPdfpopup, setOpenPdfpopup] = useState(false);
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [projectDocs, setProjectDocs] = useState<any>([]);
  const [projectData, setProjectData] = useState<any>({});
  const [dataCount, setDataCount] = useState(0);
  const [documentAdd, setDocumentAdd] = useState(false)
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const { mutate: updateProjectData } = updateProject();
  const [previewUrls, setPreviewUrls] = useState<any[]>([]);
  const [preFile, setPreFile] = useState<File[]>([]);


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
  const handleFileSelect = (e: any) => {
    const files = e.target.files;
    if (files.length > 0) {
      const fileList: File[] = Array.from(files);
      setPreFile(fileList);
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
        const selectedFileURLArray: any[] = [];
        fileList.forEach((file) => {
          selectedFilesArray.push(file);
          const urls =
            URL.createObjectURL(file)

          const obj: any = {
            name: file.name,
            url: urls
          }
          selectedFileNamesArray.push(file.name);
          selectedFileURLArray.push(obj);
        });
        setPreviewUrls(selectedFileURLArray)
        setSelectedFiles(selectedFilesArray);
        setSelectedFileName(selectedFileNamesArray);
        setFileSizeError('');
      }
    }
  };

  const viewDocument = (value: any) => {
    setOpenPdfpopup(true);
    setViewDocs(value);
  };

  const deleteFile = (index: number) => {
    const newFiles = [...selectedFiles];
    const newFileNames = [...selectedFileName];
    newFiles.splice(index, 1);
    newFileNames.splice(index, 1);
    setSelectedFiles(newFiles);
    setPreviewUrls(newFiles)
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
      setDataCount(getData?.data?.project_documents.length)
    };
    if (routeParams?.id != undefined) fetchData();
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
    if (selectedFiles?.length !== 0 || projectDocs[0]?.is_delete) {
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
            setPreviewUrls([])
            setReload(true)
            setTimeout(() => {
              navigate(`/project-edit/${data?.data?.project?.project_id}`);
              props.setLoader(props.loader);
              props.setActiveButton('PGS');
            }, 2000);
          }
        },
      });
    }
    else {
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
            <h4>DOCUMENTS</h4>
          </div>
        </div>
      </div>
      {projectDocs?.length > 0 || documentAdd === true ? (
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
                  icon={<AddIcon />}
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
            <div className={Styles.viewFiles}>
                <ol className={Styles.listStyles}>
                  {previewUrls?.map((data, index) => {                    
                    const fileName = data?.name
                    return (
                      <div className={Styles.selectedFiles}>
                        <li key={index}>
                          <div className={Styles.document}>
                            <div onClick={() => viewDocument(data?.url)} className={Styles.fileList}>
                              {fileName} {'  '}
                            </div>
                          </div>
                        </li>
                        <div className={Styles.closeIcon}>
                          <CloseIcon
                            width={10}
                            height={10}
                            onClick={() => deleteFile(index)}
                          />
                        </div>
                      </div>  
                    )
                  })}
                </ol>
              <span>
                {' '}
                <p className={Styles.errorStyles}>{fileSizeError}</p>
              </span>
            </div>
          </div>


          <div className={Styles.tableDiv}>
            {/* <div className={Styles.tableContainer}> */}
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
                          <td >{rowindex}</td>
                          <td >
                            <a href={files.path}>Document {rowindex}</a>
                          </td>
                          <td >
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

                        // <div key={index} className={Styles.iframeDiv}>
                        //   <div className={Styles.deleteIndex}>
                        //   Document {rowindex} <CloseIcon width={10} onClick={() => deleteFileinList(files.path)}/>
                        //   </div>
                        //   <iframe src={files.path} style={{maxHeight:'150%', overflowX:"auto",overflowY:"auto",maxWidth:"170px",margin:"0 auto"}} />  
                        // </div>                   
                      );
                    }
                  })
                ) : (
                  <tr>
                    <td colSpan="3" style={{ textAlign: 'center' }}>No documents found</td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>) : (
        <div>
          <div className={Styles.emptyDataHandling}>
            <div className={Styles.image}>
              <img src="/document-add.png" width="70%" height="20%" />
            </div>
            <div>
              <h5 className={Styles.textmax}>
                No documents added to this Project
              </h5>
            </div>
            <div>
              <p className={Styles.textmin}>
                Go ahead, add a document to this project now
              </p>
            </div>
            <div className={Styles.siteCreateButton}>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => {
                    setDocumentAdd(true)
                  }}
                >
                  Add Document
                </Button>
              </div>

            </div>
          </div>
        </div>
      )
      }

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
        type="success"
      />
    </div>
  );
};

export default ProjectDocument;
