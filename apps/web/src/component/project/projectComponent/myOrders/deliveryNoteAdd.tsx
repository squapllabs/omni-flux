import React, { useEffect, useState } from 'react';
import PreviousPageIcon from '../../../menu/icons/previousPageIcon';
import { useParams, useNavigate, useLocation } from 'react-router-dom';
import CustomLoader from '../../../ui/customLoader';
import Styles from '../../../../styles/newStyles/deliveryAddNote.module.scss';
import { useGetOnePurchaseOrder } from '../../../../hooks/purchase-request-hooks';
import { environment } from '../../../../environment/environment';
import Input from '../../../ui/Input';
import { formatBudgetValue } from '../../../../helper/common-function';
import { format } from 'date-fns';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import { useFormik } from 'formik';
import * as Yup from 'yup';

const MyOrderView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const {state} = useLocation();
  const projectId = state?.projectId;  
  const { data: getListData, isLoading: dataLoading } = useGetOnePurchaseOrder(
    Number(routeParams?.id)
  );
  const tableData =
    getListData?.purchase_request_data?.purchase_request_quotation_details;
  const transformedArray = tableData?.map((item: any) => ({
    item_id: item?.item_id,
    item_name: item?.item_data?.item_name,
    requested_quantity: item?.purchase_requested_quantity,
    received_quantity: 0,
  }));
  const [tableValue, setTableValue] = useState(transformedArray);
  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    const updatedTableValue = [...tableValue];
    updatedTableValue[index].received_quantity = event.target.value;
    setTableValue(updatedTableValue);
  };
  const [initialValues, setInitialValues] = useState({
    notes: '',
    invoice_number:''
  });

  const validationSchema = Yup.object().shape({
    notes: Yup.string().required('notes required'),
    invoice_number: Yup.string().required('notes required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      console.log('form called');
      console.log('values', values);
      const obj = {
        remarks: values?.notes,
      };
      console.log('obj', obj);

      //   updateOneVendorQuotes(obj, {
      //     onSuccess: (data, variables, context) => {
      //       if (data?.message === 'success') {
      //         setMessage('Vendor Approved');
      //         setOpenSnack(true);
      //         refetch();
      //         // navigate(`/purchase-request-list/${indentId}`, {
      //         //   state: { project_id: projectId },
      //         // });
      //       }
      //     },
      //   });
    },
  });
  return (
    <div className={Styles.container}>
      <CustomLoader size={48} color="#333C44">
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate(`/my-orders-view/${Number(routeParams?.id)}`,
               {state: {projectId}}
              );
            }}
          >
            <PreviousPageIcon width={20} height={20} color="#7f56d9" />
          </div>
          <div style={{ padding: '8px', display: 'flex' }}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div className={Styles.orderDetails}>
            <div className={Styles.leftOrderDetail}>
              <span>
                <b>Project Name</b>
              </span>
              <span>
                <b>Site Name</b>
              </span>
            </div>
            <div className={Styles.rightOrderDetail}>
              <p>
                <b>:</b>
              </p>
              <p>
                <b>:</b>
              </p>
            </div>
            <div className={Styles.rightOrderDetail}>
              <span>
                {getListData?.purchase_request_data?.project_data?.project_name}
              </span>
              <span>{getListData?.purchase_request_data?.site_data?.name}</span>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        {/* Input fields */}
        <div>
        <Input
              name="invoice_number"
              label="Invoice Number"
              placeholder="Enter Invoice Number"
              value={formik.values.invoice_number}
              onChange={formik.handleChange}
              mandatory={true}
              width="250px"
              error={
                formik.touched.invoice_number &&
                formik.errors.invoice_number
              }
            />
        </div>
        {/* table data */}
        <div>
          <div className={Styles.tableContainer}>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Items </th>
                  <th>Indent Requested Quantity</th>
                  <th>Received Quantity</th>
                  {/* <th>Remaining Quantity</th> */}
                </tr>
              </thead>
              <tbody>
                {tableValue?.map((items: any, index: number) => {
                  return (
                    <tr>
                      <td>{index + 1}</td>
                      <td>{items?.item_name}</td>
                      <td>{items?.requested_quantity}</td>
                      <td>
                        <Input
                          name="received_quantity"
                          value={items?.received_quantity}
                          width="100px"
                          error={false}
                          onChange={(e) => handleListChange(e, index)}
                          onKeyDown={(e) => {
                            const isNumber = /^[0-9]*$/.test(e.key);
                            if (
                              !isNumber &&
                              e.key !== 'Backspace' &&
                              e.key !== 'Delete'
                            ) {
                              e.preventDefault();
                            }
                          }}
                        />
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
          <div className={Styles.saveBtn}>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
              icon={<AddIcon color="white" />}
              //   disabled={vendorData?.length > 0 ? false : true}
              onClick={formik.handleSubmit}
            >
              Save
            </Button>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};
export default MyOrderView;
