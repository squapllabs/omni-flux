import React, { useState, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import {
  useGetOnePurchaseRequest,
  usePurchaseOrderRequest,
} from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';
import ProjectSubheader from '../project/projectSubheader';
/* Screen to view purchase request and approve it */
const PurchaseView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const PurchaseId = Number(routeParams?.id);
  /* Function to get all PO data */
  const {
    data: getAllData,
    isLoading: dataLoading,
    refetch,
  } = useGetOnePurchaseRequest(PurchaseId);
  const { mutate: postDataForFilter } = usePurchaseOrderRequest();
  /* Function toconvert PR to PO */
  const handleConvertToPo = () => {
    const purchaseOrderItems: any = [];
    getAllData?.vendor_quotes?.forEach((data: any) => {
      if (data?.quotation_status === 'Approved') {
        data?.vendor_quotation_details?.forEach((vendorQuotes: any) => {
          purchaseOrderItems.push({
            purchase_order_id: '',
            item_id: vendorQuotes?.item_id,
            order_quantity: vendorQuotes?.purchase_requested_quantity,
            unit_price: vendorQuotes?.unit_cost,
          });
        });
      }
    });
    const purchaseOrderData = {
      purchase_request_id: PurchaseId,
      vendor_id: getAllData?.selected_vendor_id,
      order_date: format(new Date(), 'yyyy-MM-dd'),
      status: 'Processing',
      total_cost: getAllData?.total_cost || 0,
      order_remark: 'Order Requested',
      purchase_order_item: purchaseOrderItems,
      purchase_order_type: 'Head Office',
    };
    postDataForFilter(purchaseOrderData, {
      onSuccess: (data, variables, context) => {
        if (data?.message === 'success') {
          setMessage('Purchase Order Create Successfull');
          setOpenSnack(true);
          setTimeout(() => {
            navigate(`/purchase-request-list/${getAllData?.indent_request_id}`);
          }, 1000);
        }
      },
    });
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  /* Function to give custom name to the files */
  const generateCustomQuotation = (data: any) => {
    if (data) {
      const vendorName = data || '';
      const year = new Date().getFullYear();
      const customBillName = `ALM-QTN-${vendorName.substring(0, 5)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };
  useEffect(() => {
    refetch();
  }, [routeParams?.id]);

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.headingTop}>
          <ProjectSubheader
            title={getAllData?.project_data?.project_name}
            navigation={`/purchase-request-list/${getAllData?.indent_request_id}`}
            description={getAllData?.project_data?.description}
          />
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.tableContainer}>
          <div>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Vendor Name </th>
                  <th>Item</th>
                  <th>Quantity</th>
                  <th>Unit Cost</th>
                  <th>Total Cost</th>
                </tr>
              </thead>
              <tbody>
                {getAllData?.vendor_quotes?.map((data: any, index: number) => {
                  if (getAllData?.selected_vendor_id === data?.vendor_id) {
                    return (
                      <>
                        {data.vendor_quotation_details.map(
                          (items: any, subIndex: any) => {
                            console.log('subItems', items);
                            return (
                              <tr>
                                <td>{subIndex + 1}</td>
                                <td>
                                  {
                                    getAllData?.selected_vendor_data
                                      ?.vendor_name
                                  }
                                </td>
                                <td>{items?.item_data?.item_name}</td>
                                <td>{items.purchase_requested_quantity}</td>
                                <td>{items.unit_cost}</td>
                                <td>{items.total_cost}</td>
                              </tr>
                            );
                          }
                        )}
                      </>
                    );
                  }
                })}
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
                  getAllData?.purchase_request_documents.map(
                    (document: any, index: number) => {
                      const customQuotationName = generateCustomQuotation(
                        getAllData?.selected_vendor_data?.vendor_name
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
              </div>
            </div>
          </div>
          <div className={Styles.approveButtons}>
            <div>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                onClick={() => handleConvertToPo()}
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
