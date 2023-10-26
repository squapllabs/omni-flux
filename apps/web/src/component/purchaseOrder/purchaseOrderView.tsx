import { useNavigate, useLocation } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import Styles from '../../styles/newStyles/purchaseOrderItemsView.module.scss';
import PreviousPageIcon from '../menu/icons/previousPageIcon';
import ProjectSubheader from '../project/projectSubheader';
import { useGetOnePurchaseOrder } from '../../hooks/purchase-request-hooks';
import { environment } from '../../environment/environment';
import CustomLoader from '../ui/customLoader';
import { formatBudgetValue } from '../../helper/common-function';

import { format } from 'date-fns';

const PurchaseOrderView = () => {
  const routeParams = useParams();
  const PurchaseOrderId = Number(routeParams?.id);
  const { data: getOnePurchaseOrderView, isLoading: dataLoading } =
    useGetOnePurchaseOrder(PurchaseOrderId);
  // console.log('vendorrr', getOnePurchaseOrderView);
  const title =
    'Purchase Order for' +
    ' ' +
    getOnePurchaseOrderView?.purchase_request_data?.project_data?.project_name;
  const description =
    getOnePurchaseOrderView?.purchase_request_data?.project_data?.description;
  // console.log('title', title);

  const tableData = getOnePurchaseOrderView?.purchase_request_data?.purchase_request_details;
  // console.log('tableData', tableData);

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
  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div>
          <ProjectSubheader
            navigation={'/purchase-order'}
            description={description}
            title={title}
          />
        </div>
      </CustomLoader>

      {/* values for order data */}
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
          <span>{getOnePurchaseOrderView?.order_id}</span>
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
                        const customQuotationName = generateCustomQuotationName(getOnePurchaseOrderView?.vendor_data)
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
                    <tr>
                      <td>{index + 1}</td>
                      <td>{item?.item_name}</td>
                      <td>
                        {item?.indent_requested_quantity || nullLableNameFromEnv}
                      </td>
                      <td>
                        {item?.purchase_requested_quantity ||
                          nullLableNameFromEnv}
                      </td>
                      <td>
                        {formatBudgetValue(item.rate ? item?.rate : 0 )}
                      </td>
                      <td>
                        {formatBudgetValue(
                          item.rate * item.purchase_requested_quantity ? item.rate * item.purchase_requested_quantity : 0
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
    </div>
  );
};
export default PurchaseOrderView;
