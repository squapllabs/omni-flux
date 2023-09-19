import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import BackArrowIcon from '../menu/icons/backArrow';
import { useGetOneOrderPurchaseRequest } from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';

const OrderView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  console.log('Purchase Request?.id)', routeParams?.id);

  const PurchaseId = Number(routeParams?.id);
  const {
    data: getAllData,
    isLoading: dataLoading,
    refetch,
  } = useGetOneOrderPurchaseRequest(PurchaseId);
  console.log('##############', getAllData);

  useEffect(() => {
    refetch();
  }, [routeParams?.id]);

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.headingTop}>
          <div className={Styles.textContent}>
            <h3>{getAllData?.purchase_request_data?.project_data?.project_name}</h3>
            <span className={Styles.content}>
              {getAllData?.purchase_request_data?.project_data?.description}
            </span>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Item Name </th>
                  <th>Item Quantity</th>
                  <th>Budget</th>
                  <th>Quotation </th>
                  <th>Bill Status</th>
                  <th>Bill</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {getAllData?.purchase_order_item?.map(
                  (data: any, index: number) => {
                    return (
                      <tr key={data.indent_request_id}>
                        <td>{index + 1}</td>
                        <td>{data?.item_data?.item_name}</td>
                        <td>{data.order_quantity}</td>  
                        <td>{data.order_quantity}</td>    
                        <td>{data.status}</td>  
                        <td>{getAllData.status}</td>  
                      </tr>
                    );
                  }
                )}
              </tbody>
            </table>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default OrderView;
