import React, { useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/customRejectPopup.module.scss';
import CustomPopup from '../ui/CustomPopupDialog';
import CloseIcon from '../menu/icons/closeIcon';
import { getIndentRejectValidateyup } from '../../helper/constants/indent-constants';
import CustomSnackBar from '../ui/customSnackBar';
import { format } from 'date-fns';
import { useUpdateIndentRequest } from '../../hooks/indent-approval-hooks';
import { useNavigate } from 'react-router-dom';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const CustomRejectIndentPopup = (props: {
  isVissible: any;
  onAction: any;
  selectedIndentId:any

}) => {
  const { isVissible, onAction,selectedIndentId } =
    props;
    
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const validationSchemaIndent = getIndentRejectValidateyup(Yup);
  const { mutate: updateIndent } = useUpdateIndentRequest();
  const [clientinitialValues, setclientInitialValues] = useState({
    approver_comments: '',
  });


  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);

  const formik = useFormik({
    initialValues: clientinitialValues,
    validationSchema: validationSchemaIndent,
    enableReinitialize: true,
    onSubmit: (values) => {
        const date = format(new Date(), 'yyyy/MM/dd');
        const Object: any = {
            approver_comments: values.approver_comments,
            indent_request_id:selectedIndentId,
            approver_status: 'Rejected',
            approved_date: null,
            rejected_date:date,
            updated_by:userID,
            approver_user_id:userID
        };
        updateIndent(Object, {
          onSuccess: (data,variables,context) => {
            // console.log('samlpe data==>', data);
            if (data?.status === true) {
              setMessage('Rejected Successfully');
              setOpenSnack(true);
              handleCloseForm();
              setTimeout(() => {
                navigate('/indent-view');
              }, 1000);
            }
          }
        })
    },
  });

  const handleCloseForm = () => {
    onAction(false);
    formik.resetForm();
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
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
                    <h4>Indent Request</h4>
                  </div>
                  <div>
                    <CloseIcon onClick={handleCloseForm} />
                  </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                <div className={Styles.inputFields}>
                  <div>
                    <Input
                      label="Comments for Rejection"
                      placeholder="Enter comments "
                      name="approver_comments"
                      mandatory={true}
                      value={formik.values.approver_comments}
                      onChange={formik.handleChange}
                      error={formik.touched.approver_comments && formik.errors.approver_comments}
                    />
                  </div>
                </div>
                <div className={Styles.dividerStyle}></div>
                <div className={Styles.formButton}>
                  <div>
                    <Button
                      // className={Styles.cancelButton}
                      color='cancel'
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
                      Submit
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

export default CustomRejectIndentPopup;
