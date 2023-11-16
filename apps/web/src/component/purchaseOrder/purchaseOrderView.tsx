import { useNavigate } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import Styles from '../../styles/newStyles/purchaseOrderItemsView.module.scss';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import { useGetOnePurchaseOrder } from '../../hooks/purchase-request-hooks';
import { environment } from '../../environment/environment';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import { useState } from 'react';
import Button from '../ui/Button';
import CustomSidePopup from '../ui/CustomSidePopup';
import GrnData from './grnData';
import CustomSnackbar from '../ui/customSnackBar';
import purchaseRequestService from '../../service/purchase-request.service';
import PurchaseOrderReport from '../reportGenerator/pdfReport/purchaseOrder';

const PurchaseOrderView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const PurchaseOrderId = Number(routeParams?.id);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [open, setOpen] = useState(false);
  const { data: getOnePurchaseOrderView, isLoading: dataLoading } =
    useGetOnePurchaseOrder(PurchaseOrderId);

  const purchase_order_id = getOnePurchaseOrderView?.purchase_order_id;

  const title =
    'Purchase Order for' +
    ' ' +
    getOnePurchaseOrderView?.purchase_request_data?.project_data?.project_name;
  const description =
    getOnePurchaseOrderView?.purchase_request_data?.project_data?.description;
  const tableData =
    getOnePurchaseOrderView?.purchase_request_data
      ?.purchase_request_quotation_details;
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;
  const generateCustomQuotationName = (data: any) => {
    if (data) {
      const vendorName = data?.vendor_name || '';
      const year = new Date().getFullYear();
      const customBillName = `ALM-${vendorName.substring(0, 5)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };

  const handleClose = () => {
    setOpenSnack(false);
  };

  const generatePDF = async (id: any) => {
    const data = await purchaseRequestService.getOnePurchaseOrderDataByID(id);
    if (data?.data) {
      await PurchaseOrderReport(data?.data);
    }
  };

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate('/purchase-order');
            }}
          >
            <PreviousPageIcon width={20} height={20} color="#7f56d9" />
          </div>
          <div style={{ display: 'flex' }}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div className={Styles.topHeader}>
            <div className={Styles.leftOrderDetail}>
              <span>
                <h4>{title}</h4>
              </span>
              <span>
                <p className={Styles.description}>{description}</p>
              </span>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
      </CustomLoader>

      {/* values for order data */}
      <div className={Styles.topHeading}>
        <div className={Styles.orderDetails}>
          <div className={Styles.leftOrderDetail}>
            <span>
              <b>Order Id</b>
            </span>
            <span>
              <b>Order Date</b>
            </span>
            <span>
              <b>Order Status</b>
            </span>
          </div>
          <div className={Styles.rightOrderDetail}>
            <p>
              <b>:</b>
            </p>
            <p>
              <b>:</b>
            </p>
            <p>
              <b>:</b>
            </p>
          </div>
          <div className={Styles.rightOrderDetail}>
            <span
              onClick={() =>
                generatePDF(getOnePurchaseOrderView?.purchase_order_id)
              }
              style={{ cursor: 'pointer', color: 'blue', fontWeight: 'bold' }}
            >
              {getOnePurchaseOrderView?.order_id}
            </span>
            <span>
              {getOnePurchaseOrderView?.order_date
                ? format(
                    new Date(getOnePurchaseOrderView?.order_date),
                    'MMM dd, yyyy'
                  )
                : '-'}
            </span>
            <span>{getOnePurchaseOrderView?.status}</span>
          </div>
        </div>
        <div className={Styles.previewButton}>
          {getOnePurchaseOrderView?.status === 'Product Received' && (
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              color="primary"
              onClick={() => {
                setOpen(true);
              }}
            >
              Preview
            </Button>
          )}
        </div>
      </div>

      {/* vendor and site details */}
      <div className={Styles.secondData}>
        <div className={Styles.vendorSiteDetails}>
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
                  {getOnePurchaseOrderView?.vendor_data?.vendor_name}{' '}
                </span>
                <span>
                  {getOnePurchaseOrderView?.vendor_data?.contact_phone_no}{' '}
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
                    getOnePurchaseOrderView?.total_cost
                      ? getOnePurchaseOrderView?.total_cost
                      : 0
                  )}{' '}
                </span>
                <span>
                  {getOnePurchaseOrderView?.purchase_request_data
                    ?.purchase_request_documents?.length > 0 ? (
                    getOnePurchaseOrderView?.purchase_request_data?.purchase_request_documents.map(
                      (document: any, index: number) => {
                        const customQuotationName = generateCustomQuotationName(
                          getOnePurchaseOrderView?.vendor_data
                        );
                        return (
                          <div key={getOnePurchaseOrderView?.purchase_order_id}>
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
              </div>
            </div>
          </div>
          <div className={Styles.dashedLine}></div>
          <div className={Styles.allDatas}>
            <div className={Styles.headingData}>
              <h3>Site Details</h3>
            </div>
            <div className={Styles.siteDetail}>
              <div className={Styles.leftSiteDetail}>
                <span>
                  <b>Name</b>
                </span>
                <span>
                  <b>Code </b>
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
                  {
                    getOnePurchaseOrderView?.purchase_request_data?.site_data
                      ?.name
                  }{' '}
                </span>
                <span>
                  {
                    getOnePurchaseOrderView?.purchase_request_data?.site_data
                      ?.code
                  }{' '}
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>
      {/* table data */}
      <div>
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S No</th>
                <th>Item Name</th>
                <th>Indent Requested Quantity</th>
                <th>Allocated Quantity</th>
                <th>Unit Price</th>
                <th>Total</th>
              </tr>
            </thead>
            <tbody>
              {tableData?.length === 0 ? (
                <tr>
                  <td colspan="4" style={{ textAlign: 'center' }}>
                    No data found
                  </td>
                </tr>
              ) : (
                tableData?.map((item: any, index: any) => {
                  return (
                    <tr key={item?.item}>
                      <td>{index + 1}</td>
                      <td>{item?.item_data?.item_name}</td>
                      <td>
                        {item?.indent_requested_quantity ||
                          nullLableNameFromEnv}
                      </td>
                      <td>
                        {item?.purchase_requested_quantity ||
                          nullLableNameFromEnv}
                      </td>
                      <td>
                        {formatBudgetValue(
                          item.unit_cost ? item?.unit_cost : 0
                        )}
                      </td>
                      <td>
                        {formatBudgetValue(
                          item.unit_cost * item.purchase_requested_quantity
                            ? item.unit_cost * item.purchase_requested_quantity
                            : 0
                        )}
                      </td>
                    </tr>
                  );
                })
              )}
            </tbody>
          </table>
        </div>
      </div>
      <CustomSidePopup
        open={open}
        title="Purchase Order Preview"
        handleClose={() => setOpen(false)}
        content={
          <GrnData
            purchaseOrderId={purchase_order_id}
            setOpen={setOpen}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
      />
      <CustomSnackbar
        open={openSnack}
        message={message}
        onClose={handleClose}
        type={'success'}
        autoHideDuration={2000}
      />
    </div>
  );
};
export default PurchaseOrderView;
