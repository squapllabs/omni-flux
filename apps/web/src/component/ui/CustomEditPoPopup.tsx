import React, { useState, useEffect, useRef } from 'react';
import { useFormik } from 'formik';
import Button from '../ui/Button';
import Styles from '../../styles/customEditPopup.module.scss';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import CustomSnackBar from '../ui/customSnackBar';
import { useUpdatePurchseOrderBillStatus } from '../../hooks/purchase-request-hooks';
import Select from '../ui/selectNew';
import PurchaseRequestService from '../../service/purchase-request.service';
import { useGetBymasertDataType } from '../../hooks/masertData-hook';

const CustomEditPoPopup = (props: {
  isVissible: any;
  onAction: any;
  selectedPurchaseOrder: any;
}) => {
  const { isVissible, onAction, selectedPurchaseOrder } = props;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const { data: getAllBillStatusTypeDatadrop = [] } =
    useGetBymasertDataType('POS');
  const { mutate: updatePoBillStatus } = useUpdatePurchseOrderBillStatus();
  const [initialValues, setInitialValues] = useState({
    bill_status: '',
  });
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [fileSizeError, setFileSizeError] = useState<string>('');
  const [selectedFileName, setSelectedFileName] = useState<string[]>([]);
  const [selectedFiles, setSelectedFiles] = useState<File[]>([]);
  const [existingFileName, setExistingFileName] = useState<string[]>([]);
  const [existingFileUrl, setExistingFileUrl] = useState<string[]>([]);
  const [docErrorMsg, setDocErrorMsg] = useState('');
  const [isFormSubmitted, setIsFormSubmitted] = useState(false);

  useEffect(() => {
    const fetchOne = async () => {
      if (selectedPurchaseOrder > 0) {
        const data = await PurchaseRequestService.getOnePurchaseOrderDataByID(
          Number(selectedPurchaseOrder)
        );
        setInitialValues({
          bill_status: data.data?.status,
        });
        const existingFileNames = data?.data?.purchase_order_documents?.map(
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
        setExistingFileUrl(data?.data?.purchase_order_documents);
      }
    };
    fetchOne();
  }, [selectedPurchaseOrder, isFormSubmitted]);

  const handleDocuments = async (
    files: File[],
    code: string,
    folder: string
  ) => {
    try {
      const uploadPromises = files.map(async (file) => {
        const response = await PurchaseRequestService.documentUpload(
          file,
          code,
          folder
        );
        return response.data;
      });
      const uploadResponses = await Promise.all(uploadPromises);
      const modifiedArray = uploadResponses.flatMap((response) => response);
      const modifiedArrayWithDeleteFlag = modifiedArray?.map((obj) => ({
        ...obj,
        is_delete: false,
      }));
      if (existingFileUrl?.length > 0 && selectedFiles?.length > 0) {
        existingFileUrl.forEach((item) => {
          item.is_delete = true;
        });
        const combinedArray =
          modifiedArrayWithDeleteFlag.concat(existingFileUrl);
        return combinedArray;
      } else if (existingFileUrl?.length > 0) {
        return existingFileUrl;
      } else {
        return modifiedArrayWithDeleteFlag;
      }
    } catch (error) {
      console.log('Error in occur project document upload:', error);
    }
  };

  const formik = useFormik({
    initialValues: initialValues,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      const s3UploadUrl = await handleDocuments(
        selectedFiles,
        `purchase-order-item-${selectedPurchaseOrder}`,
        'purchase-order-item'
      );
      const Object: any = {
        status: values.bill_status,
        purchase_order_documents:
          s3UploadUrl && s3UploadUrl.length > 0 ? s3UploadUrl : existingFileUrl,
        purchase_order_id: Number(selectedPurchaseOrder),
        updated_by: 1,
      };
      if (Object.status === 'Completed' || Object.status === 'Invoice') {
        if (Object.purchase_order_documents?.length > 0) {
          updatePoBillStatus(Object, {
            onSuccess: (data, variables, context) => {
              if (data?.status === true) {
                setMessage('Purchase order edited');
                setOpenSnack(true);
                setIsFormSubmitted(true);
                handleCloseForm();
                resetForm();
              }
            },
          });
        } else {
          setDocErrorMsg('Document is mandatory for the above status');
        }
      } else {
        updatePoBillStatus(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setMessage('Purchase order edited');
              setOpenSnack(true);
              setIsFormSubmitted(true);
              handleCloseForm();
              resetForm();
            }
          },
        });
      }
    },
  });

  const handleCloseForm = () => {
    onAction(false);
    setSelectedFileName([]);
    setFileSizeError('');
    formik.resetForm();
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
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
      setDocErrorMsg('');
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
        setDocErrorMsg('');
      }
    }
  };

  return (
    <div>
      <div>
        {isVissible && (
          <CustomPopup className="sample">
            <div className={Styles.popupContent}>
              <form onSubmit={formik.handleSubmit}>
                <div className={Styles.header}>
                  <div>
                    <h4>Edit PO</h4>
                  </div>
                  <div>
                    <CloseIcon onClick={handleCloseForm} />
                  </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                <div className={Styles.mainField}>
                  <div>
                    <Select
                      label="Bill Status"
                      defaultLabel="Select the Status"
                      placeholder="Select the Status"
                      name="bill_status"
                      onChange={formik.handleChange}
                      value={formik.values.bill_status}
                      width="250px"
                    >
                      {getAllBillStatusTypeDatadrop?.map((option: any) => (
                        <option
                          key={option.master_data_id}
                          value={option.master_data_name}
                        >
                          {option.master_data_name}
                        </option>
                      ))}
                    </Select>
                  </div>
                </div>
                <div className={Styles.topDocumnetLayer}>
                  <p className={Styles.documentheading}>
                    Bill (If the bill status is invoice or completed *)
                  </p>
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
                <div className={Styles.dividerStyleOne}></div>
                <div className={Styles.formButton}>
                  <div>
                    <Button
                      className={Styles.cancelButton}
                      shape="rectangle"
                      justify="center"
                      size="small"
                      onClick={handleCloseForm}
                    >
                      Cancel
                    </Button>
                  </div>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      type="submit"
                    >
                      Save
                    </Button>
                  </div>
                </div>
              </form>
            </div>
          </CustomPopup>
        )}
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

export default CustomEditPoPopup;
