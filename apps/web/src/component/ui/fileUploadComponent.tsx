import React, { useEffect, useRef, useState } from 'react';
import Button from './Button';
import UploadIcon from '../menu/icons/cloudUpload';
import AddIcon from '../menu/icons/addIcon';
import CloseIcon from '../menu/icons/closeIcon';
import Styles from '../../styles/project.module.scss';
import FileUploadIcon from '../menu/icons/fileUploadIcon';

interface FileUploadProps {
  handleDrop: (e: React.DragEvent<HTMLDivElement>) => void;
  viewDocument: (url: string) => void;
  onSelectedFilesChange: (files: File[]) => void;
  refreshView: boolean;
  setRefreshView: React.Dispatch<React.SetStateAction<boolean>>;
  enableDrop: boolean;
}

const FileUploadComponent: React.FC<FileUploadProps> = ({
  handleDrop,
  viewDocument,
  onSelectedFilesChange,
  refreshView,
  setRefreshView,
  enableDrop,
}) => {
  const fileInputRef = useRef<HTMLInputElement>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [previewUrls, setPreviewUrls] = useState<any[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);

  const handleInternalFileSelect = (e: any) => {
    if (fileInputRef.current) {
      (fileInputRef.current as HTMLInputElement).click();
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
        const selectedFileURLArray: any[] = [];
        fileList.forEach((file) => {
          selectedFilesArray.push(file);
          const urls = URL.createObjectURL(file);
          const obj: any = {
            name: file.name,
            url: urls,
          };
          selectedFileNamesArray.push(file.name);
          selectedFileURLArray.push(obj);
        });
        setPreviewUrls(selectedFileURLArray);
        setSelectedFiles(selectedFilesArray);
        setSelectedFileName(selectedFileNamesArray);
        setFileSizeError('');
        onSelectedFilesChange(selectedFilesArray);
      }
    }
  };

  const deleteFile = (index: number) => {
    const newFiles = [...selectedFiles];
    const newFileNames = [...selectedFileName];
    newFiles.splice(index, 1);
    newFileNames.splice(index, 1);
    setSelectedFiles(newFiles);
    setPreviewUrls(newFiles);
    setSelectedFileName(newFileNames);
  };

  useEffect(() => {
    if (refreshView) {
      setRefreshView(false);
      setPreviewUrls([]);
      setSelectedFiles([]);
      setSelectedFileName([]);
    }
  }, [refreshView]);

  return (
    <div>
      {enableDrop === true ? (
        <div className={Styles.container_document}>
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
                      JPG, PNG, or PDF, file size no more than 10MB
                    </span>
                  </div>
                  <input
                    ref={fileInputRef}
                    id="upload-photo"
                    name="upload_photo"
                    type="file"
                    style={{ display: 'none' }}
                    onChange={(e) => handleFileSelect(e)}
                    multiple
                  />
                  <Button
                    onClick={(e) => {
                      handleInternalFileSelect(e);
                    }}
                    type="button"
                    shape="rectangle"
                    size="small"
                  >
                    Add Files
                  </Button>
                </div>
              </div>
              <div className={Styles.viewFiles}>
                <ol className={Styles.listStyles}>
                  {previewUrls?.map((data, index) => {
                    const fileName = data?.name;
                    return (
                      <div className={Styles.selectedFiles} key={index}>
                        <li>
                          <div className={Styles.document}>
                            <div
                              onClick={() => viewDocument(data?.url)}
                              className={Styles.fileList}
                            >
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
                    );
                  })}
                </ol>
                <span>
                  {' '}
                  <p className={Styles.errorStyles}>{fileSizeError}</p>
                </span>
              </div>
            </div>
          </div>
        </div>
      ) : (
        <div>
          <div title="Attach document">
            <input
              ref={fileInputRef}
              id="upload-photo"
              name="upload_photo"
              type="file"
              style={{ display: 'none' }}
              onChange={(e) => handleFileSelect(e)}
            />
            <div
              style={{
                cursor: 'pointer',
                padding: '15px 0x 10px 5px',
              }}
              onClick={(e) => {
                handleInternalFileSelect(e);
              }}
            >
              <FileUploadIcon color="#7f56d9"/>
            </div>
          </div>
          <div className={Styles.viewFiles}>
            <ol className={Styles.listStyles}>
              {previewUrls?.map((data, index) => {
                const fileName = data?.name;
                return (
                  <div className={Styles.selectedFiles} key={index}>
                    <li>
                      <div className={Styles.document}>
                        <div
                          onClick={() => viewDocument(data?.url)}
                          className={Styles.fileList}
                        >
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
                );
              })}
            </ol>
            <span>
              {' '}
              <p className={Styles.errorStyles}>{fileSizeError}</p>
            </span>
          </div>
        </div>
      )}
    </div>
  );
};

export default FileUploadComponent;
