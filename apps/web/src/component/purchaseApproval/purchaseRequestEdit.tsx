import React, { useState, useEffect } from 'react';
import purchaseRequestService from '../../service/purchaseRequest-service';
import { useFormik } from 'formik';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import UploadIcon from '../menu/icons/cloudUpload';
import Styles from '../../styles/purchaseEdit.module.scss';

const PurchaseRequestEdit: React.FC = (props: any) => {
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
      console.log('data', data);

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
            label="Total Cost"
            placeholder="Enter total cost"
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
                  //   onDrop={(e) => handleDrop(e)}
                  onDragOver={(e) => e.preventDefault()}
                >
                  <h6>Select a file or drag and drop here</h6>
                  <span className={Styles.documentSpan}>
                    JPG,PNG or PDF, file size no more than 10MB
                  </span>
                </div>
                <input
                  //   ref={fileInputRef}
                  id="upload-photo"
                  name="upload_photo"
                  type="file"
                  style={{ display: 'none' }}
                  //   onChange={handleFileSelect}
                  multiple
                />
                <Button
                  //   onClick={onButtonClick}
                  type="button"
                  shape="rectangle"
                  size="small"
                >
                  Add Files
                </Button>
              </div>
            </div>
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
