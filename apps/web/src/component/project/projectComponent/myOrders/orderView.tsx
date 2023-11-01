import React, { useEffect, useState, useRef } from 'react';
import PreviousPageIcon from '../../../menu/icons/previousPageIcon';
import { useParams, useNavigate, useLocation } from 'react-router-dom';
import CustomLoader from '../../../ui/customLoader';
import Styles from '../../../../styles/newStyles/myorderView.module.scss';
import { useGetOnePurchaseOrder } from '../../../../hooks/purchase-request-hooks';
import { environment } from '../../../../environment/environment';
import { formatBudgetValue } from '../../../../helper/common-function';
import { format } from 'date-fns';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import ViewIcon from '../../../menu/icons/viewIcon';
import { useGetAllGrnData } from '../../../../hooks/grn-hooks';

const MyOrderView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const { state } = useLocation();
  const projectId = state?.projectId;
  const { data: getListData, isLoading: dataLoading } = useGetOnePurchaseOrder(
    Number(routeParams?.id)
  );
  const purchaseOrderId = Number(routeParams?.id)
  // console.log('UUUUUUU', getListData);
  const tableData =
    getListData?.purchase_request_data?.purchase_request_quotation_details;
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const object: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'asc',
    project_id: projectId
  };
  const {
    isLoading: getAllLoadingLabourData,
    data: GRData,
    refetch,
  } = useGetAllGrnData(object);

  const generateCustomQuotationName = (data: any) => {
    if (data) {
      const vendorName = data?.vendor_name || '';
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

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
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
                {getListData?.purchase_request_data?.project_data?.project_name}
              </span>
              <span>{getListData?.purchase_request_data?.site_data?.name}</span>
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
                    <b>Order Id</b>
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
                  <span>{getListData?.order_id}</span>
                  <span>
                    {getListData?.order_date
                      ? format(
                        new Date(getListData?.order_date),
                        'MMM dd, yyyy'
                      )
                      : '-'}
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
                      getListData?.total_cost ? getListData?.total_cost : 0
                    )}{' '}
                  </span>
                  <span>
                    {getListData?.purchase_request_data
                      ?.purchase_request_documents?.length > 0 ? (
                      getListData?.purchase_request_data?.purchase_request_documents.map(
                        (document: any, index: number) => {
                          const customQuotationName =
                            generateCustomQuotationName(
                              getListData?.vendor_data
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
                  <span>{getListData?.vendor_data?.vendor_name} </span>
                  <span>{getListData?.vendor_data?.contact_phone_no} </span>
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
                              ? item.unit_cost *
                              item.purchase_requested_quantity
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
          <div className={Styles.addDeliveryBtn}>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              icon={<AddIcon width={20} color="white" />}
              onClick={() => {
                navigate(`/delivery-note/${purchaseOrderId}`,
                  { state: { projectId } }
                );
              }}
            >
              Add Delivery Notes
            </Button>
          </div>
        </div>
        <div className={Styles.dashedDivider}></div>
        <div>
          <div className={Styles.headingForTable}>
            <h3>Received Goods</h3>
          </div>
          <div>
            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>S No</th>
                    <th>Goods Received Date</th>
                    <th>Total Items</th>
                    <th>Options</th>
                  </tr>
                </thead>
                <tbody>
                  {GRData?.length === 0 ? (
                    <tr>
                      <td colspan="4" style={{ textAlign: 'center' }}>
                        No data found
                      </td>
                    </tr>
                  ) : (
                    GRData?.content?.map((item: any, index: any) => {
                      return (
                        <tr>
                          <td>{index + 1}</td>
                          <td>{dateFormat(item?.goods_received_date)}</td>
                          <td>{item?.grn_details?.length}</td>
                          <td>
                            <ViewIcon
                              onClick={() => {
                                navigate(`/view-received-goods/${purchaseOrderId}/${item?.grn_id}`,
                                  { state: { projectId } }
                                );
                              }}
                            />
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
      </CustomLoader>
    </div>
  );
};
export default MyOrderView;
