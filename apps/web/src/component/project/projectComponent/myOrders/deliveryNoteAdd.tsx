import React, { useEffect, useState, useRef } from 'react';
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
import DatePicker from '../../../ui/CustomDatePicker';
import AddIcon from '../../../menu/icons/addIcon';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import TextArea from '../../../ui/CustomTextArea';
import userService from '../../../../service/user-service';
import FileUploadIcon from '../../../menu/icons/fileUploadIcon';
import CloseIcon from '../../../menu/icons/closeIcon';
import CustomSnackBar from '../../../ui/customSnackBar';
import { createGrn } from '../../../../hooks/grn-hooks';

const MyOrderView = () => {
  const routeParams = useParams();
  const purchaseOrderId = Number(routeParams?.id);
  const navigate = useNavigate();
  const { state } = useLocation();
  const projectId = state?.projectId;
  const { data: getListData, isLoading: dataLoading } = useGetOnePurchaseOrder(
    Number(routeParams?.id)
  );
  const { mutate: postGrnData } = createGrn();
  const year = new Date().getFullYear();
  const tableData =
    getListData?.purchase_request_data?.purchase_request_quotation_details;
  const transformedArray = tableData?.map((item: any) => ({
    item_id: item?.item_id,
    item_name: item?.item_data?.item_name,
    requested_quantity: item?.purchase_requested_quantity,
    received_quantity: 0,
  }));
  const [tableValue, setTableValue] = useState(transformedArray);
  const [invoiceDocument, setInvoiceDocument] = useState<any>([]);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
//   console.log('invoice document', invoiceDocument);

  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    const updatedTableValue = [...tableValue];
    updatedTableValue[index].received_quantity = Number(event.target.value);
    setTableValue(updatedTableValue);
  };

  const currentDate = new Date();
  const [initialValues, setInitialValues] = useState({
    notes: '',
    invoice_number: '',
    goods_received_date: currentDate.toISOString().slice(0, 10),
  });
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const onButtonClick = () => {
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };

  const handleFileSelect = async (e: any) => {
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
        console.log('msg', errorMessage);
      } else {
        const arr: any = [];
        fileList.forEach(async (file) => {
          const code = 'INVOICE';
          const response = await userService.documentUpload(file, code);
        //   console.log('response', response?.data[0]);
          const obj = {
            ...response?.data[0],
            is_delete: 'N',
          };
          //   console.log('responseobj', obj);
          arr.push(obj);
        });
        setInvoiceDocument(arr);
        setMessage('Document uploaded');
        setOpenSnack(true);
      }
    }
  };

  const generateCustomQuotationName = (data: any) => {
    // console.log('func innn');
    if (data) {
      const year = new Date().getFullYear();
      const customBillName = `ALM-${data.substring(0, 3)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };

  const deleteFileinList = () => {
    setInvoiceDocument([]);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const validationSchema = Yup.object().shape({
    notes: Yup.string().required('Notes Required'),
    invoice_number: Yup.string().required('Invoice Reference Number Required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      console.log('form called');
      console.log('values', values);
      const obj = {
        notes: values?.notes,
        invoice_id: values?.invoice_number,
        grn_details: tableValue,
        purchase_order_id: Number(routeParams?.id),
        goods_received_date: values.goods_received_date,
        bill_details: invoiceDocument,
        goods_received_by: 1,
        grn_status: 'Pending',
        project_id: 195,
        created_by: 1,
      };
      console.log('obj', obj);
      if (invoiceDocument?.length > 0) {
        console.log('inn okay');
        postGrnData(obj, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Posted');
              setOpenSnack(true);
              navigate(`/my-orders-view/${Number(routeParams?.id)}`, {
                state: { projectId },
              });
            }
          },
        });
      } else {
        setMessage('Bill is Mandatory');
        setOpenSnack(true);
      }
    },
  });
  return (
    <div className={Styles.container}>
      <CustomLoader loader={dataLoading} size={48} color="#333C44">
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate(`/my-orders-view/${Number(routeParams?.id)}`, {
                state: { projectId },
              });
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
        <div className={Styles.inputField}>
          <div>
            <Input
              name="invoice_number"
              label="Invoice Reference Id"
              placeholder="Enter Invoice Number"
              value={formik.values.invoice_number}
              onChange={formik.handleChange}
              mandatory={true}
              width="250px"
              error={
                formik.touched.invoice_number && formik.errors.invoice_number
              }
            />
          </div>
          <div>
            <DatePicker
              label="Delivered Date"
              name="goods_received_date"
              mandatory={true}
              value={formik.values.goods_received_date}
              onChange={formik.handleChange}
              InputProps={{
                inputProps: {
                  min: '1930-01-01',
                  max: `${new Date().toISOString().slice(0, 10)}`,
                },
              }}
              error={
                formik.touched.goods_received_date &&
                formik.errors.goods_received_date
              }
            />
          </div>
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
              gap: '10px',
            }}
          >
            <div className={Styles.uploadLabel}>Upload Invoice</div>
            {invoiceDocument?.length > 0 &&
            invoiceDocument[0].is_delete === 'N' ? (
              <div>
                {invoiceDocument?.map((document: any, index: any) => {
                  const customQuotationName =
                    generateCustomQuotationName('Invoice');
                  if (document.is_delete === 'N')
                    return (
                      <div
                        key={document.code}
                        style={{
                          width: '150px',
                          cursor: 'pointer',
                          fontWeight: 'bolder',
                          color: 'blue',
                          display: 'flex',
                          fontSize: '15px',
                        }}
                      >
                        <div>
                          <a
                            href={document.path}
                            target="_blank"
                            rel="noopener noreferrer"
                          >
                            {customQuotationName}
                          </a>
                        </div>
                        <CloseIcon
                          width={5}
                          height={10}
                          onClick={() => deleteFileinList()}
                        />
                      </div>
                    );
                })}
              </div>
            ) : (
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
                    paddingBottom: '5px',
                  }}
                  onClick={() => {
                    onButtonClick();
                  }}
                >
                  <FileUploadIcon color="#7f56d9" />
                </div>
              </div>
            )}
          </div>
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
          <div className={Styles.inputField}>
            <TextArea
              name="notes"
              label="Comments"
              rows={5}
              maxCharacterCount={1000}
              value={formik.values.notes}
              onChange={formik.handleChange}
              width="500px"
              mandatory={true}
              error={formik.touched.notes && formik.errors.notes}
            />
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
export default MyOrderView;
