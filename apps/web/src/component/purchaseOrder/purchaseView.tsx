import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import { useGetOnePurchaseRequest,purchaseOrderRequest } from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';

const PurchaseView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  console.log('routeParams?.id)', routeParams?.id);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const PurchaseId = Number(routeParams?.id);
  const {
    data: getAllData,
    isLoading: dataLoading,
    refetch,
  } = useGetOnePurchaseRequest(PurchaseId);
  console.log('llllllllll', getAllData);
  const {
    mutate: postDataForFilter,
  } = purchaseOrderRequest();
  const constructPurchaseOrder = () => {
    const purchaseOrderItems = [];
    getAllData?.purchase_request_details?.forEach((data: any) => {
      purchaseOrderItems.push({
        purchase_order_id: '',
        item_id: data.item_id,
        order_quantity: data.quantity,
      });
    });

    const purchaseOrderData = {
      purchase_request_id: PurchaseId,
      vendor_id: getAllData?.selected_vendor_id,
      order_date: format(new Date(), 'yyyy-MM-dd'),
      status: 'Processing',
      total_cost: getAllData?.total_cost || 0,
      order_remark: 'Order Requested',
      purchase_order_item: purchaseOrderItems,
    };
    console.log('sampel', purchaseOrderData);
    postDataForFilter(purchaseOrderData, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Purchase Order Create Successfull');
            setOpenSnack(true);
            setTimeout(() => {
              navigate(`/purchase-order`);
            }, 1000);
          }
        },
      });
  };

  const handleConvertToPo = () => {
    constructPurchaseOrder();
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
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
          <div className={Styles.rightContent}>
            <h3>Estimated Delivery Date :</h3>
            <span>20-10-2023</span>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Vendor Name </th>
                  <th>Item</th>
                  <th>Quantity</th>
                </tr>
              </thead>
              <tbody>
                {getAllData?.purchase_request_details?.map(
                  (data: any, index: number) => {
                    return (
                      <tr key={data.indent_request_id}>
                        <td>{index + 1}</td>
                        <td>{getAllData?.selected_vendor_data?.vendor_name}</td>
                        <td>{data.item_name}</td>
                        <td>{data.quantity}</td>
                      </tr>
                    )
                  }
                )}
              </tbody>
            </table>
          </div>
          <div className={Styles.quatationStyle}>
            <div>
              <h4>Quotation Budget</h4>
              <h4>Quotation</h4>
            </div>
            <div>
              <div>:</div>
              <div>:</div>
            </div>
            <div>
              <div>
                {formatBudgetValue(
                  getAllData?.total_cost ? getAllData?.total_cost : 0
                )}
              </div>
              <div>
                {getAllData?.purchase_request_documents?.length > 0 ? (
                  getAllData.purchase_request_documents.map(
                    (document: any, index: number) => (
                      <div key={document.code}>
                        <a
                          href={document.path}
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          Document {index + 1}
                        </a>
                      </div>
                    )
                  )
                ) : (
                  <div>No documents available</div>
                )}
              </div>
            </div>
          </div>
          <div className={Styles.approveButtons}>
            <div>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="secondary"
                // onClick={() => ()}
              >
                Back
              </Button>
            </div>
            <div>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                onClick={handleConvertToPo}
              >
                Convert To Po
              </Button>
            </div>
          </div>
        </div>
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </CustomLoader>
    </div>
  );
};

export default PurchaseView;
