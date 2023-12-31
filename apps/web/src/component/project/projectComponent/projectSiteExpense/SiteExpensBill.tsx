import React, { useRef, useState, useEffect } from 'react';
import Styles from '../../../../styles/newStyles/projectSiteExpense.module.scss';
import CloseIcon from '../../../menu/icons/closeIcon';
import Button from '../../../ui/Button';
import userService from '../../../../service/user-service';
import DeleteIcon from '../../../menu/icons/deleteIcon';

const SiteExpenseBill: React.FC = (props: any) => {
  let rowindex = 0;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  /* Function to upload files via drag and drop */
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
  /* Function to select file */
  const handleFileSelect = async (e: any) => {
    const files = e.target.files;
    props.setLoader(true);
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
        props.setLoader(false);
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
        props.setLoader(false);
      }
    }
  };
  /* Function to remove a file after selecting */
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
    const code = 'SITEEXPENSE' + props.projectId;
    const s3UploadUrl: any = await handleDocuments(
      selectedFiles,
      code.toUpperCase()
    );
    props.setExpenseBill([...props.expenseBill, ...s3UploadUrl]);
  };

  return (
    <div style={{ display: 'flex' }}>
      <div style={{ width: '50%' }}>
        <div className={Styles.documentContainer}>
          <div className={Styles.documentOuterLayer}>
            <div className={Styles.documentContent}>
              <div
                id="drop-area"
                onDrop={(e) => handleDrop(e)}
                onDragOver={(e) => e.preventDefault()}
              >
                <h4>Upload Bill / Invoice copy</h4>
                <span className={Styles.documentSpan}>
                  Max file size is 500kb. Supported file types are .jpg and
                  .png.
                </span>
              </div>
              <div>
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
                  color="primary"
                >
                  Browse
                </Button>
              </div>
            </div>
            <div>
              <div
                style={{
                  display: 'flex',
                  gap: '10px',
                  paddingLeft: '20px',
                }}
              >
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
      </div>
      <div style={{ width: '50%' }}>
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
                    <td colSpan="4" style={{ textAlign: 'center' }}>
                      No document found
                    </td>
                  </tr>
                ) : (
                  ''
                )}
                {props.expenseBill?.map((files: any, index: any) => {
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
