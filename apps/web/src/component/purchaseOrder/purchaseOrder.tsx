import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import BackArrowIcon from '../menu/icons/backArrow';
import { useGetOnePurchaseRequest } from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';

const IndentView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  console.log('routeParams?.id)', routeParams?.id);
  const [purchaseOrderData, setPurchaseOrderData] = useState({
    purchase_request_id: '',
    vendor_id: '',
    order_date: format(new Date(), 'yyyy-MM-dd'),
    status: 'Pending',
    total_cost: '',
    order_remark: 'Order Requested',
    purchase_order_item: [],
  });
  const PurchaseId = Number(routeParams?.id);
  const {
    data: getAllData,
    isLoading: dataLoading,
    refetch,
  } = useGetOnePurchaseRequest(PurchaseId);
  console.log('llllllllll', getAllData);

  useEffect(() => {
    refetch();
  }, [routeParams?.id]);

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.headingTop}>
          <div className={Styles.textContent}>
            <h3>{getAllData?.project_data?.project_name}</h3>
            <span className={Styles.content}>
              {getAllData?.project_data?.description}
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
                {/* {getAllData?.purchase_request_details?.map(
                  (data: any, index: number) => {
                    return (
                      <tr key={data.indent_request_id}>
                        <td>{index + 1}</td>
                        <td>{getAllData?.selected_vendor_data?.vendor_name}</td>
                        <td>{data.item_name}</td>
                        <td>{data.quantity}</td>
                      </tr>
                    );
                  }
                )} */}
              </tbody>
            </table>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default IndentView;
