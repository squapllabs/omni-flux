import React, { useRef, useState, useEffect } from 'react';
import purchaseRequestService from '../../service/purchaseRequest-service';
import { useFormik } from 'formik';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import Styles from '../../styles/purchaseEdit.module.scss';
import userService from '../../service/user-service';

const PurchaseRequestEdit: React.FC = (props: any) => {
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  let PR_;
  const [initialValues, setInitialValues] = useState({
    purchase_request_id: '',
    total_cost: '',
    purchase_request_documents: '',
  });
  useEffect(() => {
    const fetchOne = async () => {
      const data = await purchaseRequestService.getOneProjectRequestById(
        props.purchaseID
      );
      setInitialValues({
        purchase_request_id: data?.data?.purchase_request_id,
        total_cost: data?.data?.total_cost,
        purchase_request_documents: data?.data?.purchase_request_documents,
      });
    };
    fetchOne();
  }, []);

  const handleClose = () => {
    props.setOpen(false);
  };

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

  const handleDocuments = async (
    files: File[],
    purchase_request_id: string
  ) => {
    try {
      const uploadPromises = files.map(async (file) => {
        const response = await userService.documentUpload(
          file,
          purchase_request_id
        );
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
      console.log('Error in occur purchase document upload:', error);
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

  const formik = useFormik({
    initialValues,
    onSubmit: async (values) => {
      const s3UploadUrl = await handleDocuments(
        selectedFiles,
        'PR_'+'values.purchase_request_id'
      );
      const Object: any = {
        purchase_request_id: values.purchase_request_id,
        total_cost: values.total_cost,
        purchase_request_documents: s3UploadUrl,
      };
    },
  });

  return (
    <div className={Styles.formContainer}>
      <form>
        <div className={Styles.header}>
          <div>
            <h4 className={Styles.titleStyle}>Edit Purchase Request</h4>
          </div>
          <div>
            <CancelIcon onClick={handleClose} />
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Input
            name="total_cost"
            label="Budget"
            placeholder="Enter Budget"
            // value={formik.values.total_cost}
            // onChange={formik.handleChange}
            // error={
            //   formik.touched.total_cost && formik.errors.total_cost
            // }
            width="100%"
          />
        </div>
        <div className={Styles.field}>
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
                  width={'100%'}
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
                {selectedFileName.map((fileName, index) => (
                  <li key={index} style={{ paddingTop: '5px' }}>
                    {fileName} {'    '}
                    <CancelIcon
                      // width={10}
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
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.formButton}>
          <div>
            <Button
              className={Styles.cancelButton}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleClose}
            >
              Cancel
            </Button>
          </div>
          <div>
            <Button
              type="submit"
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
            >
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};
export default PurchaseRequestEdit;
