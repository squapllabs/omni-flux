import React, { useState, useEffect, useRef } from 'react';
import { useFormik } from 'formik';
import Button from '../ui/Button';
import Styles from '../../styles/customEditInvoicePopup.module.scss';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import CustomSnackBar from '../ui/customSnackBar';
import { updatePurchseOrderBillStatus } from '../../hooks/purchase-request-hooks';
import Select from '../ui/selectNew';
import PurchaseRequestService from '../../service/purchase-request.service';
import { useGetBymasertDataType } from '../../hooks/masertData-hook';
import DatePicker from '../ui/CustomDatePicker';
import { editInvoiceValidateyup } from '../../helper/constants/invoice-constants';
import * as Yup from 'yup';
import { formatBudgetValue } from '../../helper/common-function';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const CustomEditInvoicePopup = (props: {
  isVissible: any;
  onAction: any;
  selectedPurchaseOrder: any;
  selectedInvoive: any;
  selectedInvoiceId: any;
  selectedInvAmt: any;
  selectedInvDoc: any;
}) => {
  const {
    isVissible,
    onAction,
    selectedPurchaseOrder,
    selectedInvoive,
    selectedInvoiceId,
    selectedInvAmt,
    selectedInvDoc,
  } = props;
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const { data: getAllBillStatusTypeDatadrop = [] } =
  useGetBymasertDataType('PYS');
  const { data: getAllPaymentTypeDatadrop = [] } = useGetBymasertDataType('PPM');

  const { mutate: updatePoBillStatus } = updatePurchseOrderBillStatus();
  const [initialValues, setInitialValues] = useState({
    bill_status: '',
    payment_date: '',
    payment_mode: '',
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
          payment_mode: data?.data?.payment_mode,
          payment_date: data?.data?.payment_date,
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

  const validationSchema = editInvoiceValidateyup(Yup);
  const formik = useFormik({
    initialValues: initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      // console.log('inn');
      const s3UploadUrl = await handleDocuments(
        selectedFiles,
        `purchase-order-item-${selectedPurchaseOrder}`,
        'purchase-order-item'
      );
      const Object: any = {
        status: 'Paid',
        payment_mode: values.payment_mode,
        paid_date: new Date(),
        additional_info:
          s3UploadUrl && s3UploadUrl.length > 0 ? s3UploadUrl : undefined,
        purchase_order_id: Number(selectedPurchaseOrder),
        updated_by: Number(userID),
        paid_by: Number(userID),
        purchase_order_invoice_id: Number(selectedInvoiceId),
      };
      // console.log('object', Object);
      updatePoBillStatus(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.status === true) {
            setMessage('Payment Process is completed');
            setOpenSnack(true);
            setIsFormSubmitted(true);
            handleCloseForm();
            resetForm();
          }
        },
      });
    },
  });

  const handleCloseForm = () => {
    // onAction(false);
    setSelectedFileName([]);
    setFileSizeError('');
    formik.resetForm();
    setTimeout(() => {
      handleClose();
    }, 1000);
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
  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.divOne}>
          <div className={Styles.invDiv}>
            <h5>Invoice</h5>
            <span>:</span>
            <p className={Styles.invoiceNumber}>
              <a
                href={selectedInvDoc}
                target="_blank"
                rel="noopener noreferrer"
              >
                {selectedInvoive}
              </a>
            </p>
          </div>
          <div className={Styles.invDiv}>
            <h5>Amount</h5>
            <span>:</span>
            <p className={Styles.invoiceNumber}>
              {formatBudgetValue(selectedInvAmt ? selectedInvAmt : 0)}
            </p>
          </div>
          <div>
            <div>
              <Select
                label="Payment Mode"
                defaultLabel="Select the payment mode"
                placeholder="Select the payment mode"
                name="payment_mode"
                onChange={formik.handleChange}
                value={formik.values.payment_mode}
                width="250px"
                error={
                  formik.touched.payment_mode && formik.errors.payment_mode
                }
              >
                {getAllPaymentTypeDatadrop?.map((option: any) => (
                  <option
                    key={option.master_data_id}
                    value={option.master_data_name}
                  >
                    {option.master_data_name}
                  </option>
                ))}
              </Select>
            </div>
            <div className={Styles.topDocumnetLayer}>
              <p className={Styles.documentheading}>Reference documents</p>
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
          </div>
        </div>

        <div className={Styles.footer}>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.button}>
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleClose}
              className={Styles.cancelButton}
            >
              Cancel
            </Button>
            <Button
              shape="rectangle"
              color="primary"
              justify="center"
              size="small"
              type="submit"
            >
              Paid
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

export default CustomEditInvoicePopup;
