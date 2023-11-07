import React, { useEffect, useState, useRef } from 'react';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import { useParams, useNavigate, useLocation } from 'react-router-dom';
import CustomLoader from '../ui/customLoader';
import Styles from '../../styles/newStyles/newInvoiceView.module.scss';
import { useGetOnePurchaseOrder } from '../../hooks/purchase-request-hooks';
import { environment } from '../../environment/environment';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import ViewIcon from '../menu/icons/viewIcon';
import { getByPurchaseOrderId } from '../../hooks/invoice-hooks';
import EditIcon from '../menu/icons/newEditIcon';
import CustomPopup from '../ui/CustomSidePopup';
import CustomEditInvoicePopup from '../ui/CustomEditInvoicePopup';
import ReportGenerator from '../reportGenerator/pdfReport/invoice';
import PdfDownloadIcon from '../menu/icons/pdfDownloadIcon';

const MyOrderView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const { state } = useLocation();
  const projectId = state?.projectId;
  const purchaseOrderId = Number(routeParams?.id);
  const [invoiceNumber, setInvoiceNumber] = useState();
  const [purchaseId, setPurchaseId] = useState();
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [invoiceId, setInvoiceId] = useState(false);
  const [reload, setReload] = useState(false);
  const { isLoading: dataLoading, data: getAllData = [] } =
    getByPurchaseOrderId(purchaseOrderId);
  console.log('ooooooooopppppppp', getAllData);

  const generateCustomInvoice = (data: any) => {
    if (data) {
      const vendorName = data || '';
      const year = new Date().getFullYear();
      const customBillName = `ALM-${vendorName.substring(0, 5)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd-MM-yyyy');
    return formattedDate;
  };

  const handleEdit = (value: any, invoice: any, invoiceId: any) => {
    setPurchaseId(value);
    setInvoiceNumber(invoice);
    setInvoiceId(invoiceId);
    setOpen(true);
  };

  const handleClosePopup = () => {
    setOpen(false);
  };

  //   useEffect(() => {
  //     refetch();
  //   }, []);

  const handleReportGenerator = async (data: any) => {
    const obj: any = {
      title: 'Invoice and Payments',
      name: 'invoice',
      ...data,
    };
    ReportGenerator(obj);
  };

  return (
    <div className={Styles.container}>
      <CustomLoader loader={dataLoading} size={48} color="#333C44">
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate(`/finance-view`);
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
                {
                  getAllData[0]?.purchase_order_data?.purchase_request_data
                    ?.project_data?.project_name
                }
              </span>
              <span>
                {
                  getAllData[0]?.purchase_order_data?.purchase_request_data
                    ?.site_data?.name
                }
              </span>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        {/* main data */}
        <div className={Styles.secondData}>
          <div className={Styles.vendorSiteDetails}>
            <div className={Styles.allDatas}>
              <div className={Styles.headingData}>
                <h3>Order Details</h3>
              </div>
              <div className={Styles.siteDetail}>
                <div className={Styles.leftSiteDetail}>
                  <span>
                    <b>PO Id</b>
                  </span>
                  <span>
                    <b>Order Date </b>
                  </span>
                </div>
                <div className={Styles.rightSiteDetail}>
                  <p>
                    <b>:</b>
                  </p>
                  <p>
                    <b>:</b>
                  </p>
                </div>
                <div className={Styles.rightSiteDetail}>
                  <span>{getAllData[0]?.purchase_order_data?.order_id}</span>
                  <span>
                    {dateFormat(
                      getAllData[0]?.purchase_order_data?.created_date
                        ? getAllData[0]?.purchase_order_data?.created_date
                        : new Date()
                    )}
                  </span>
                </div>
              </div>
            </div>
            <div className={Styles.dashedLine}></div>
            <div className={Styles.allDatas}>
              <div className={Styles.headingData}>
                <h3>Quotation Details</h3>
              </div>
              <div className={Styles.siteDetail}>
                <div className={Styles.leftSiteDetail}>
                  <span>
                    <b>Budget</b>
                  </span>
                  <span>
                    <b>Quotation </b>
                  </span>
                </div>
                <div className={Styles.rightSiteDetail}>
                  <p>
                    <b>:</b>
                  </p>
                  <p>
                    <b>:</b>
                  </p>
                </div>
                <div className={Styles.rightSiteDetail}>
                  <span>
                    {formatBudgetValue(
                      getAllData[0]?.purchase_order_data?.purchase_request_data
                        ?.total_cost
                        ? getAllData[0]?.purchase_order_data
                            ?.purchase_request_data?.total_cost
                        : 0
                    )}
                  </span>
                  <span>
                    {getAllData[0]?.purchase_order_data?.purchase_request_data
                      ?.purchase_request_documents?.length > 0 ? (
                      getAllData[0]?.purchase_order_data?.purchase_request_data?.purchase_request_documents.map(
                        (document: any, index: number) => {
                          const customQuotationName = generateCustomInvoice(
                            getAllData[0]?.purchase_order_data?.vendor_data
                              ?.vendor_name
                          );
                          return (
                            <div key={document.code}>
                              <a
                                href={document.path}
                                target="_blank"
                                rel="noopener noreferrer"
                              >
                                {customQuotationName}
                              </a>
                            </div>
                          );
                        }
                      )
                    ) : (
                      <div>-</div>
                    )}
                  </span>
                  {/* <span>
                    {getAllData[0]?.purchase_order_data?.purchase_request_data
                      ?.purchase_request_documents?.length > 0 ? (
                      getAllData[0]?.purchase_order_data?.purchase_request_data?.purchase_request_documents.map(
                        (document: any, index: number) => (
                          <div key={document.code}>
                            <a
                              href={document.path}
                              target="_blank"
                              rel="noopener noreferrer"
                            >
                              {customQuotationName}
                            </a>
                          </div>
                        )
                      )
                    ) : (
                      <div>-</div>
                    )}
                  </span> */}
                </div>
              </div>
            </div>
            <div className={Styles.dashedLine}></div>
            <div className={Styles.allDatas}>
              <div className={Styles.headingData}>
                <h3>Vendor Details</h3>
              </div>
              <div className={Styles.vendorDetails}>
                <div className={Styles.leftVendorDetail}>
                  <span>
                    <b>Name</b>
                  </span>
                  <span>
                    <b>Contact Number </b>
                  </span>
                </div>
                <div className={Styles.rightVendorDetail}>
                  <p>
                    <b>:</b>
                  </p>
                  <p>
                    <b>:</b>
                  </p>
                </div>
                <div className={Styles.rightVendorDetail}>
                  <span>
                    {
                      getAllData[0]?.purchase_order_data?.purchase_request_data
                        ?.selected_vendor_data?.vendor_name
                    }
                  </span>
                  <span>
                    {
                      getAllData[0]?.purchase_order_data?.purchase_request_data
                        ?.selected_vendor_data?.contact_phone_no
                    }
                  </span>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.dashedDivider}></div>
        <div className={Styles.headingForTable}>
          <h3>Invoice List</h3>
        </div>
        <div>
          <div className={Styles.tableContainer}>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Goods Received Date</th>
                  <th>Invoice No</th>
                  <th>Invoice Document</th>
                  <th>Paid By</th>
                  <th>Payment Date</th>
                  <th>Payment Method</th>
                  <th>Status</th>
                  <th>Options</th>
                </tr>
              </thead>
              <tbody>
                {getAllData ? (
                  getAllData?.map((item: any, index: any) => {
                    const customBillName = generateCustomInvoice(
                      item?.purchase_order_data?.vendor_data?.vendor_name
                    );
                    return (
                      <tr>
                        <td>{index + 1}</td>
                        <td>
                          {dateFormat(item?.grn_data?.goods_received_date)}
                        </td>
                        <td>{item?.invoice_number}</td>
                        <td>
                          <div>
                            {item?.invoice_document.map(
                              (document: any, index: number) => (
                                <div key={document.code}>
                                  <a
                                    href={document.path}
                                    target="_blank"
                                    rel="noopener noreferrer"
                                  >
                                    {customBillName}
                                  </a>
                                </div>
                              )
                            )}
                          </div>
                        </td>
                        <td>
                          {item?.status === 'Paid'
                            ? item?.paid_by_data?.first_name +
                              ' ' +
                              item?.paid_by_data?.last_name
                            : '-'}
                        </td>
                        <td>
                          {item?.status === 'Paid'
                            ? dateFormat(item?.paid_date)
                            : '-'}
                        </td>
                        <td>
                          {item?.status === 'Paid' ? item?.payment_mode : '-'}
                        </td>
                        <td>{item?.status}</td>
                        <td>
                          <div style={{ display: 'flex', gap: '10px' }}>
                            {item?.status === 'To Be Paid' ? (
                              <EditIcon
                                onClick={() =>
                                  handleEdit(
                                    item.purchase_order_id,
                                    item?.grn_data?.invoice_id,
                                    item?.purchase_order_invoice_id
                                  )
                                }
                              />
                            ) : (
                              ' '
                            )}
                            <div>
                              <PdfDownloadIcon
                                onClick={() => handleReportGenerator(item)}
                              />
                            </div>
                          </div>
                        </td>
                      </tr>
                    );
                  })
                ) : (
                  <tr>
                    <td colspan="4" style={{ textAlign: 'center' }}>
                      No data found
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
        <CustomPopup
          title="Edit Payment Details"
          open={open}
          handleClose={handleClosePopup}
          content={
            <CustomEditInvoicePopup
              setOpen={setOpen}
              open={open}
              setReload={setReload}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
              selectedPurchaseOrder={purchaseId}
              selectedInvoive={invoiceNumber}
              selectedInvoiceId={invoiceId}
            />
          }
        />
      </CustomLoader>
    </div>
  );
};
export default MyOrderView;
