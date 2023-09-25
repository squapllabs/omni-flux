import React, { useRef, useState, useEffect } from 'react';
import Styles from '../../styles/expanses.module.scss';
import CloseIcon from '../menu/icons/closeIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import Button from '../ui/Button';
import userService from '../../service/user-service';
import DeleteIcon from '../menu/icons/deleteIcon';

const SiteExpenseBill: React.FC = (props: any) => {
  let rowindex = 0;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  //   const [projectDocs, setProjectDocs] = useState<any>([]);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
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
  const handleFileSelect = async (e: any) => {
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
    const objectIndex = props.expenseBill.findIndex(
      (obj: any) => obj.path === data
    );
    props.expenseBill[objectIndex] = {
      ...props.expenseBill[objectIndex],
      is_delete: 'Y',
    };
    props.setExpenseBill([...props.expenseBill]);
    rowindex = rowindex - 1;
  };
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };
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
    let code = 'SITEEXPENSE' + props.projectId;
    const s3UploadUrl: any = await handleDocuments(
      selectedFiles,
      code.toUpperCase()
    );
    // console.log('s3UploadUrl', s3UploadUrl);
    props.setExpenseBill([...props.expenseBill, ...s3UploadUrl]);
  };
  // console.log('props.expenseBill', props.expenseBill);

  return (
    <div style={{ display: 'flex' }}>
      <div style={{ width: '50%' }}>
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
              Upload
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
      </div>
      <div style={{ width: '30%' }}>
        <div
          style={{
            width: '100%',
            display: 'flex',
            justifyContent: 'flex-start',
          }}
        >
          <div className={Styles.table_container}>
            <table className={Styles.scrollable_table}>
              <tr>
                <th>SI No</th>
                <th>Documents</th>
                <th>Action</th>
              </tr>
              <tbody>
                {props.expenseBill?.length === 0 ? (
                  <tr>
                    <td colSpan="4" style={{ textAlign: 'center' }}>No document found</td>
                  </tr>
                ) : (
                  ''
                )}
                {props.expenseBill?.map((files: any, index: any) => {
                  console.log('files', files);
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
      </div>
    </div>
  );
};

export default SiteExpenseBill;
