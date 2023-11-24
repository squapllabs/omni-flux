import React, { useState } from 'react';
import Styles from '../../styles/projectSettings.module.scss';
import Button from '../ui/Button';
import Select from '../ui/selectNew';
import { useUpdateIndentRequest } from '../../hooks/indent-approval-hooks';
import format from 'date-fns/format';
import { useNavigate } from 'react-router-dom';

const IndentApprovePopup = (props: any) => {
  const navigate = useNavigate();
  const { mutate: updateIndentRequestData } = useUpdateIndentRequest();
  const purchase_type: any = [
    { value: 'Select from Options', label: 'Select from Options' },
    { value: 'Local Purchase', label: 'Local Purchase' },
    { value: 'Head Office', label: 'Head Office' },
  ];
  const [purchaseType, setPurchaseType] = useState('');
  /* Function to approve the indent */
  const handleApprove = () => {
    const date = format(new Date(), 'yyyy/MM/dd');
    const obj = {
      indent_request_id: props.indentId,
      approver_status: 'Approved',
      request_type: purchaseType,
      approved_date: date,
      rejected_date: null,
      updated_by: props.userId,
      approver_user_id: props.userId,
    };
    updateIndentRequestData(obj, {
      onSuccess: (data, variables, context) => {
        if (data?.status === true) {
          props.setMessage('Approved Successfully');
          props.setOpenSnack(true);
          setTimeout(() => {
            navigate('/indent-view');
          }, 1000);
          props.setOpen(false);
        }
      },
    });
  };

  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div>
      <div className={Styles.divOne}>
        <div>
          <div>
            <Select
              label="Purchase Order Type"
              name="request_type"
              placeholder="Select from options"
              mandatory={true}
              width="350px"
              onChange={(e) => setPurchaseType(e.target.value)}
            >
              {purchase_type?.map((items: any, index: any) => {
                return (
                  <option key={items.value} value={items.value}>
                    {items.label}
                  </option>
                );
              })}
            </Select>
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
            color='cancel'
            // className={Styles.cancelButton}
          >
            Cancel
          </Button>
          <Button
            shape="rectangle"
            color="primary"
            justify="center"
            size="small"
            type="submit"
            onClick={handleApprove}
          >
            Save
          </Button>
        </div>
      </div>
    </div>
  );
};

export default IndentApprovePopup;
