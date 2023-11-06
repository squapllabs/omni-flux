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

const MyOrderView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const { state } = useLocation();
  const projectId = state?.projectId;
  const purchaseOrderId = Number(routeParams?.id);

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

//   useEffect(() => {
//     refetch();
//   }, []);

  return (
    <div className={Styles.container}>
      <CustomLoader size={48} color="#333C44">
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
              <span>name</span>
              description
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
                  <span>KKK</span>
                  <span>ASD</span>
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
                  <span>dsd</span>
                  <span>dsds</span>
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
                  <span>dsds</span>
                  <span>dsds</span>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.dashedDivider}></div>
        <div className={Styles.headingForTable}>
          <h3>Invoice List</h3>
        </div>
      </CustomLoader>
    </div>
  );
};
export default MyOrderView;
