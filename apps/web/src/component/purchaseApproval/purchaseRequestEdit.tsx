import React, { useRef, useState, useEffect } from 'react';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import { useFormik } from 'formik';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import Styles from '../../styles/purchaseEdit.module.scss';
import vendorQuotesService from '../../service/vendorQuotes-service';
import { updateVendorQuotes } from '../../hooks/vendorQuotes-hooks';
import * as Yup from 'yup';
import { getCreateValidateyup } from '../../helper/constants/vendorSelect-constants';

const PurchaseRequestEdit: React.FC = (props: any) => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const { mutate: updateOneVendorQuotes } = updateVendorQuotes();
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [existingFileName, setExistingFileName] = useState<string[]>([]);
  const [docErrorMsg, setDocErrorMsg] = useState('');
  const [existingFileUrl, setExistingFileUrl] = useState<string[]>([]);
  const [initialValues, setInitialValues] = useState({
    vendor_quotes_id: '',
    total_quotation_amount: '',
    purchase_request_id: '',
    vendor_id: '',
    quotation_status:'',

  });
  const validationSchema = getCreateValidateyup(Yup);
  useEffect(() => {
    const fetchOne = async () => {
      const data = await vendorQuotesService.getOneVendorQuotesById(
        props.vendorID
      );
      console.log('data', data);
      setInitialValues({
        vendor_quotes_id: data?.data?.vendor_quotes_id,
        total_quotation_amount: data?.data?.total_quotation_amount,
        purchase_request_id: data?.data?.purchase_request_id,
        vendor_id: data?.data?.vendor_id,
        quotation_status: data?.data?.quotation_status
      });
      const existingFileNames = data?.data?.vendor_quotes_documents.map(
        (document: any) => {
          const pathParts = document.path.split('/');
          const fileName = pathParts[pathParts.length - 1];
          const originalFileNameMatches = fileName.match(/-.*-(.*\.\w+)/);
          if (originalFileNameMatches) {
            return originalFileNameMatches[1];
          }
          return fileName;
        }
      );
      setExistingFileName(existingFileNames);
      setExistingFileUrl(data?.data?.vendor_quotes_documents);
    };
    fetchOne();
  }, [props.vendorID]);

  console.log('initialValues', initialValues);

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

  const handleDocuments = async (files: File[], code: string, folder: string) => {
    try {
      const uploadPromises = files.map(async (file) => {
        const response = await vendorQuotesService.documentUpload(
          file,
          code,
          folder
        );
        return response.data;
      });
      const uploadResponses = await Promise.all(uploadPromises);
      const modifiedArray = uploadResponses.flatMap((response) => response);
      const modifiedArrayWithDeleteFlag = modifiedArray.map((obj) => ({
        ...obj,
        is_delete: false,
      }));
      if (existingFileUrl.length > 0 && selectedFiles.length>0) {
        existingFileUrl.forEach((item) => {
          item.is_delete = false;
        });
        const combinedArray =
          modifiedArrayWithDeleteFlag.concat(existingFileUrl);
        return combinedArray;
      }
      else if(existingFileUrl.length > 0) {
        return existingFileUrl
      }
       else {
        return modifiedArrayWithDeleteFlag;
      }
    } catch (error) {
      console.log('Error in occur purchase document upload:', error);
    }
  };
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      const s3UploadUrl = await handleDocuments(
        selectedFiles,
        `purchase-request-${props.vendorID}`,
        'project-1-purchase-request-1'
      );
      const Object: any = {
        vendor_quotes_id: values.vendor_quotes_id,
        total_quotation_amount: Number(values.total_quotation_amount),
        purchase_request_id: values.purchase_request_id,
        vendor_id: values.vendor_id,
        quotation_status:'Quotation Recieved',
        vendor_quotes_documents: s3UploadUrl && s3UploadUrl.length > 0 ? s3UploadUrl : existingFileUrl,
        updated_by: userID
      };
      console.log('Object', Object);

      updateOneVendorQuotes(Object, {
        onSuccess: (data, variables, context) => {
          console.log(data);

          if (data?.message === 'success') {
            props.setMessage('Vendor Quotation added');
            props.setOpenSnack(true);
            props.setOpen(false);
            props.setReload(true);
          }
        },
      });
    },
  });

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
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
            name="total_quotation_amount"
            label="Budget"
            placeholder="Enter Budget"
            value={formik.values.total_quotation_amount}
            onChange={formik.handleChange}
            width="100%"
            error={formik.touched.total_quotation_amount && formik.errors.total_quotation_amount}
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
                  // multiple
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
            {/* <span>
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
            </span> */}
            {selectedFileName?.length === 0 ? (
              <span>
                <ol className={Styles.listStyles}>
                  {existingFileName?.map((fileName, index) => (
                    <ol key={index}>{fileName}</ol>
                  ))}
                </ol>
              </span>
            ) : (
              <span>
                <ol className={Styles.listStyles}>
                  {selectedFileName?.map((fileName, index) => (
                    <ol key={index}>{fileName}</ol>
                  ))}
                </ol>
              </span>
            )}
            <span>
              {' '}
              <p className={Styles.errorStyles}>{fileSizeError}</p>
              <p className={Styles.errorStyles}>{docErrorMsg}</p>
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
