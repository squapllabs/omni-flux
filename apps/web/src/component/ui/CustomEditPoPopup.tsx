import React, { useState, useEffect,useRef } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Button from '../ui/Button';
import Styles from '../../styles/customEditPopup.module.scss';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import { getBomValidateyup } from '../../helper/constants/bom-constants';
import CustomSnackBar from '../ui/customSnackBar';
import {
  useGetMasterBillStatusParentType,
  updatePurchseOrderBillStatus,
} from '../../hooks/purchase-request-hooks';
import Select from '../ui/selectNew';
import purchaseRequestService from '../../service/purchase-request.service';

const CustomEditPoPopup = (props: {
  isVissible: any;
  onAction: any;
  selectedPurchaseOrder: any;
}) => {
  const { isVissible, onAction, selectedPurchaseOrder } = props;
  console.log('id props', selectedPurchaseOrder);
  console.log('id type', typeof selectedPurchaseOrder);
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const { data: getAllBillStatusTypeDatadrop = [] } =
    useGetMasterBillStatusParentType();
  const { mutate: updatePoBillStatus } = updatePurchseOrderBillStatus();
  const [initialValues, setInitialValues] = useState({
    bill_status: '',
    bill_document: '',
  });
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);

  useEffect(() => {
    const fetchOne = async () => {
      const data = await purchaseRequestService.getOnePurchaseOrderDataByID(
        Number(selectedPurchaseOrder)
      );
      console.log('@@@@@@@@@@@', data.status);
      setInitialValues({
        bill_status: data?.data?.status,
        bill_document: data?.data?.purchase_order_documents,
      });
    };
    fetchOne();
  }, [selectedPurchaseOrder]);

  const formik = useFormik({
    initialValues: initialValues,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        bill_status: values.bill_status,
        bill_document: '',
      };
      //   updateCategoryData(Object, {
      //     onSuccess: (data,variables,context) => {
      //       console.log('samlpe data==>', data);
      //       if (data?.status === true) {
      //         setMessage('Abstract edited');
      //         setOpenSnack(true);
      //         handleCloseForm();
      //         resetForm();
      //       }
      //     }
      //   })
      console.log('+++++++++++++++', Object);
    },
  });

  const handleCloseForm = () => {
    onAction(false);
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
    // e.preventDefault();
    // e.stopPropagation();
    // const files = e.dataTransfer.files;
    // const fileList = Array.from(files);
    // const oversizedFiles = fileList.filter(
    //   (file) => file.size > 10 * 1024 * 1024
    // );
    // if (oversizedFiles.length > 0) {
    //   const oversizedFileNames = oversizedFiles
    //     .map((file) => file.name)
    //     .join(', ');
    //   const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
    // //   setFileSizeError(errorMessage);
    // } else {
    //   const selectedFilesArray: File[] = [];
    //   const selectedFileNamesArray: string[] = [];

    //   fileList.forEach((file) => {
    //     selectedFilesArray.push(file);
    //     const originalFileNameMatch = file.name.match(/-(\d+-\d+-.*)$/);
    //     const originalFileName = originalFileNameMatch
    //       ? originalFileNameMatch[1]
    //       : file.name;
    //     selectedFileNamesArray.push(originalFileName);
    //   });
    //   setSelectedFiles(selectedFilesArray);
    //   setSelectedFileName(selectedFileNamesArray);
    //   setFileSizeError('');
    // }
  };

  const handleFileSelect = (e: any) => {
    const files = e.target.files;
    // if (files.length > 0) {
    //   const fileList: File[] = Array.from(files);
    //   const oversizedFiles = fileList.filter(
    //     (file) => file.size > 10 * 1024 * 1024
    //   );
    //   if (oversizedFiles.length > 0) {
    //     const oversizedFileNames = oversizedFiles
    //       .map((file) => file.name)
    //       .join(', ');
    //     const errorMessage = `The following files exceed 10MB: ${oversizedFileNames}`;
    //     setFileSizeError(errorMessage);
    //   } else {
    //     const selectedFilesArray: File[] = [];
    //     const selectedFileNamesArray: string[] = [];
    //     fileList.forEach((file) => {
    //       selectedFilesArray.push(file);
    //       selectedFileNamesArray.push(file.name);
    //     });
    //     setSelectedFiles(selectedFilesArray);
    //     setSelectedFileName(selectedFileNamesArray);
    //     setFileSizeError('');
    //   }
    // }
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
                      name="bill_status"
                      placeholder="Select the Status"
                    //   mandatory={true}
                      width="250px"
                      onChange={formik.handleChange}
                      value={formik.values.bill_status}
                      defaultLabel="Select from options"
                    >
                      {getAllBillStatusTypeDatadrop.map((option: any) => (
                        <option key={option.value} value={option.value}>
                          {option.label}
                        </option>
                      ))}
                    </Select>
                  </div>
                </div>
                <div className={Styles.topDocumnetLayer}>
                    <p className={Styles.documentheading}>Bill (If the bill status is invoice or completed *)</p>
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
