import React, { useEffect, useState, useRef } from 'react';
import PreviousPageIcon from '../../../menu/icons/previousPageIcon';
import { useParams, useNavigate, useLocation } from 'react-router-dom';
import CustomLoader from '../../../ui/customLoader';
import Styles from '../../../../styles/newStyles/deliveryAddNote.module.scss';
import {
  useGetOnePurchaseOrder,
  useGetOnePurchaseOrderTableData,
} from '../../../../hooks/purchase-request-hooks';
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
import poService from '../../../../service/purchase-request.service';
import FileUploadIcon from '../../../menu/icons/fileUploadIcon';
import CloseIcon from '../../../menu/icons/closeIcon';
import CustomSnackBar from '../../../ui/customSnackBar';
import { useCreateGrn } from '../../../../hooks/grn-hooks';
import { store, RootState } from '../../../../redux/store';
import { getToken } from '../../../../redux/reducer';
import purchaseRequestService from '../../../../service/purchase-request.service';
import PurchaseOrderReport from '../../../reportGenerator/pdfReport/purchaseOrder';
import DeliveryNotes from '../../../reportGenerator/pdfReport/deliveryNotes';

const MyOrderView = () => {
  const routeParams = useParams();
  const purchaseOrderId = Number(routeParams?.id);
  const navigate = useNavigate();
  const { state } = useLocation();
  const projectId = state?.projectId;
  const getState: RootState = store.getState();
  const encryptedData = getToken(getState, 'Data');
  const userID: number = encryptedData.userId;
  const { data: getListData, isLoading: dataLoading } = useGetOnePurchaseOrder(
    Number(routeParams?.id)
  );
  const { mutate: postGrnData } = useCreateGrn();
  const [tableValue, setTableValue] = useState<any>([]);
  const [invoiceDocument, setInvoiceDocument] = useState<any>([]);
  const [openSnack, setOpenSnack] = useState(false);
  const [loaderData, setLoaderData] = useState(true);
  const [errors, setErrors] = useState<Array<string>>(
    new Array(tableValue.length).fill('')
  );
  const [message, setMessage] = useState('');
  const [isButtonDisabled, setIsButtonDisabled] = useState(false);
  const [allRecieved, setAllRecieved] = useState(false);
  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    let tempObj = {};
    tempObj = {
      ...tableValue[index],
      [event.target.name]: Number(event.target.value),
    };
    const tempArry = [...tableValue];
    tempArry[index] = tempObj;
    setTableValue(tempArry);
    if (
      tempArry[index].inward_remaining_quantity <
      tempArry[index].currently_received_quantity
    ) {
      setErrors((prevErrors) => {
        const newErrors = [...prevErrors];
        newErrors[index] = true;
        return newErrors;
      });
    } else {
      setErrors((prevErrors) => {
        const newErrors = [...prevErrors];
        newErrors[index] = false;
        return newErrors;
      });
    }
  };

  const order_complete = tableValue?.every(
    (tableValue: {
      currently_received_quantity: number;
      inward_remaining_quantity: number;
    }) =>
      tableValue?.currently_received_quantity ===
      tableValue?.inward_remaining_quantity
  );
  const currentDate = new Date();
  const [initialValues, setInitialValues] = useState({
    notes: '',
    invoice_number: '',
    goods_received_date: currentDate.toISOString().slice(0, 10),
    invoice_amount: '',
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
      } else {
        const arr: any = [];
        fileList.forEach(async (file) => {
          const code = 'INVOICE';
          const response = await userService.documentUpload(file, code);
          const obj = {
            ...response?.data[0],
            is_delete: 'N',
          };
          arr.push(obj);
        });
        setInvoiceDocument(arr);
        setMessage('Document uploaded');
        setOpenSnack(true);
      }
    }
  };

  const generateCustomQuotationName = (data: any) => {
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
    goods_received_date: Yup.date()
      .required('Date is Required')
      .max(new Date(), 'Future Date not allowed'),
    invoice_amount: Yup.number().required('Amount is Required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      setIsButtonDisabled(true);
      setTimeout(() => {
        setIsButtonDisabled(false);
      }, 5000);
      const obj = {
        notes: values?.notes,
        invoice_id: values?.invoice_number,
        grn_details: tableValue,
        purchase_order_id: Number(routeParams?.id),
        goods_received_date: values.goods_received_date,
        bill_details: invoiceDocument,
        goods_received_by: userID,
        grn_status: 'Pending',
        project_id: projectId,
        site_id:
          getListData?.purchase_order_type === 'Head Office'
            ? getListData?.purchase_request_data?.site_id
            : getListData?.indent_request_data?.site_id,
        created_by: userID,
        purchase_order_type: getListData?.purchase_order_type,
        invoice_amount: Number(values?.invoice_amount),
        is_product_received: order_complete,
      };
      if (errors.includes(true)) {
        setMessage('Mismatch quantity');
        setOpenSnack(true);
      } else if (invoiceDocument?.length === 0) {
        setMessage('Bill is Mandatory');
        setOpenSnack(true);
      } else {
        // If none of the above conditions are met, execute this block
        postGrnData(obj, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              const invoiceData: any = {
                tableData: tableValue,
                invoiceNumber:
                  data?.data?.grn_with_grn_details?.grn?.invoice_id,
                notes: data?.data?.grn_with_grn_details?.grn?.notes,
              };

              setMessage('Goods delivered added');
              DeliveryNotes(getListData, invoiceData);
              setOpenSnack(true);
              setTimeout(() => {
                navigate(`/project-edit/${Number(state?.projectId)}`);
              }, 1000);
            }
          },
        });
      }
    },
  });

  useEffect(() => {
    const fetchData = async () => {
      const data = await poService.getOnePurchaseOrderTableDataByID(
        Number(routeParams?.id)
      );
      const arr: any = [];
      data?.map((items: any, index: any) => {
        const obj = {
          ...items,
          currently_received_quantity: 0,
        };
        arr.push(obj);
      });
      setTableValue(arr);
      setLoaderData(false);
    };
    fetchData();
  }, []);

  const generatePDF = async (id: any) => {
    const data = await purchaseRequestService.getOnePurchaseOrderDataByID(id);
    if (data?.data) {
      await PurchaseOrderReport(data?.data);
    }
  };

  return (
    <div className={Styles.container}>
      <CustomLoader loader={loaderData} size={48} color="#333C44">
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate(`/project-edit/${Number(state?.projectId)}`);
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
                {getListData?.purchase_order_type === 'Head Office'
                  ? getListData?.purchase_request_data?.project_data
                      ?.project_name
                  : getListData?.indent_request_data?.project_data
                      ?.project_name}
              </span>
              <span>
                {getListData?.purchase_order_type === 'Head Office'
                  ? getListData?.purchase_request_data?.site_data?.name
                  : getListData?.indent_request_data?.site_data?.name}
              </span>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        {/* Input fields */}
        <div className={Styles.inputField}>
          <div className={Styles.inputContainer}>
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
              <Input
                name="invoice_amount"
                width="120px"
                mandatory={true}
                label="Amount"
                // error={errors[index]}
                onChange={formik.handleChange}
                onKeyDown={(e) => {
                  const isNumber = /^[0-9]*$/.test(e.key);
                  const isArrowKey =
                    e.key === 'ArrowLeft' || e.key === 'ArrowRight';
                  if (
                    !isNumber &&
                    !isArrowKey &&
                    e.key !== 'Backspace' &&
                    e.key !== 'Delete'
                  ) {
                    e.preventDefault();
                  }
                }}
                error={
                  formik.touched.invoice_amount && formik.errors.invoice_amount
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
          {/* <div> */}
          <div className={Styles.purchaseOrderDetails}>
            <div className={Styles.leftPurchaseOrderDetail}>
              <span>
                <b>Purchase Order</b>
              </span>
            </div>
            <div className={Styles.righPurchasetOrderDetail}>
              <p>
                <b>:</b>
              </p>
            </div>
            <div className={Styles.righPurchasetOrderDetail}>
              <span
                onClick={() => generatePDF(getListData?.purchase_order_id)}
                style={{ cursor: 'pointer', color: 'blue', fontWeight: 'bold' }}
              >
                {getListData?.order_id}
              </span>
            </div>
          </div>
          {/* </div> */}
        </div>
        {/* table data */}
        <div>
          <div className={Styles.tableContainer}>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Items </th>
                  <th>Allocated Quantity</th>
                  <th>Previously Received</th>
                  <th>To Be Received</th>
                  <th>Currently Received</th>
                  {/* <th>Remaining Quantity</th> */}
                </tr>
              </thead>
              <tbody>
                {tableValue?.map((items: any, index: number) => {
                  return (
                    <tr>
                      <td>{index + 1}</td>
                      <td>{items?.item_name}</td>
                      {/* Items */}
                      <td>{items?.order_quantity}</td>
                      {/* Allocated quantity */}
                      <td>{items?.previously_received_quantity}</td>
                      {/* previously received */}
                      <td>{items?.inward_remaining_quantity}</td>
                      {/* To be received */}
                      <td>
                        <Input
                          name="currently_received_quantity"
                          value={items?.currently_received_quantity}
                          width="100px"
                          // error={errors[index]}
                          errorDisable={true}
                          borderError={errors[index] ? true : false}
                          error={errors[index] ? true : false}
                          onChange={(e) => handleListChange(e, index)}
                          onKeyDown={(e) => {
                            const isNumber = /^[0-9]*$/.test(e.key);
                            const isArrowKey =
                              e.key === 'ArrowLeft' || e.key === 'ArrowRight';
                            if (
                              !isNumber &&
                              !isArrowKey &&
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
          <div className={Styles.inputFieldTextArea}>
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
              disabled={isButtonDisabled}
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
