import React, { useEffect, useState } from 'react';
import Button from '../ui/Button';
import grnService from '../../service/grn-service';
import Styles from '../../styles/newStyles/purchaseOrderItemsView.module.scss';
import { format } from 'date-fns';
import ExpandClose from '../menu/icons/expandClose';
import ExpandIcon from '../menu/icons/expandIcon';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import { updatePurchseOrderStatus } from '../../hooks/purchase-request-hooks';
import { useNavigate } from 'react-router-dom';
const GrnData: React.FC = (props: any) => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID = encryptedData.userId;
  const navigate = useNavigate();
  const [grnData, setGrnData] = useState<any>([]);
  const [colps, setColps] = useState(false);
  const [poID, setPoID] = useState<any>({});
  const { mutate: updatePoBillStatus } = updatePurchseOrderStatus();

  useEffect(() => {
    const fetchOne = async () => {
      const data = await grnService.getGrnByPOId(props?.purchaseOrderId);
      setGrnData(data?.data);
    };
    fetchOne();
  }, [props?.purchaseOrderId]);

  const handleExpand = async (data: any) => {
    setColps(!colps);
    if (colps === true) {
      setPoID(data);
    } else {
      setPoID({});
    }
  };

  const handleSubmit = () => {
    const Object: any = {
      status: 'Invoice',
      purchase_order_id: Number(props?.purchaseOrderId),
      updated_by: userID,
    };
    updatePoBillStatus(Object, {
      onSuccess: (data, variables, context) => {
        if (data?.status === true) {
          setTimeout(() => {
            navigate('/purchase-order');
          }, 2000);
          props.setOpenSnack(true);
          props.setMessage('Purchase Order moved to Invoice');
        }
      },
    });
  };

  const downloadFile = (file: any) => {
    const url = file?.path;
    const link = document.createElement('a');
    link.href = url;
    link.click();
  };

  return (
    <div>
      <div className={Styles.tableContainer}>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th></th>
              <th>S No</th>
              <th>Goods Recieved Date</th>
              <th>Invoice</th>
              <th>Delivery Notes</th>
            </tr>
          </thead>
          <tbody>
            {grnData?.map((data: any, index: any) => {
              return (
                <>
                  <tr key={data?.grn_id}>
                    <td>
                      <div
                        onClick={() => {
                          handleExpand(data);
                        }}
                        style={{
                          display:
                            data?.grn_details?.length !== 0 ? '' : 'none',
                        }}
                      >
                        {colps === false && data?.grn_id === poID?.grn_id ? (
                          <ExpandClose />
                        ) : (
                          <ExpandIcon />
                        )}
                      </div>
                    </td>
                    <td>{index + 1}</td>
                    <td>
                      {format(
                        new Date(data?.goods_received_date),
                        'MMM dd, yyyy'
                      )}
                    </td>
                    <td>
                      <div>
                        {data?.bill_details.length > 0 ? (
                          data?.bill_details.map(
                            (document: any, index: number) => (
                              <div onClick={() => downloadFile(document)}>
                                {/* <a
                                  href={document.path} download
                                  target="_blank"
                                  rel="noopener noreferrer"
                                >
                                  
                                </a> */}
                                {data?.invoice_id}
                              </div>
                            )
                          )
                        ) : (
                          <div>-</div>
                        )}
                      </div>
                      {/* {data?.invoice_id} */}
                    </td>
                    <td>{data?.notes}</td>
                  </tr>
                  {data?.grn_id === poID?.grn_id && (
                    <tr>
                      <td colSpan="6">
                        {/* <div className={Styles.subTableContainer}> */}
                        <table className={Styles.scrollable_sub_table}>
                          <thead>
                            <tr>
                              <th>S. No</th>
                              <th>Item Name</th>
                              <th>Quantity</th>
                            </tr>
                          </thead>
                          <tbody>
                            {data?.grn_details?.map((item: any, index: any) => {
                              return (
                                <tr key={item?.grn_details_id}>
                                  <td>{index + 1}</td>
                                  <td>{item?.item_data?.item_name}</td>
                                  <td>{item?.received_quantity}</td>
                                </tr>
                              );
                            })}
                          </tbody>
                        </table>
                        {/* </div> */}
                      </td>
                    </tr>
                  )}
                </>
              );
            })}
          </tbody>
        </table>
      </div>

      <div className={Styles.footer}>
        <div className={Styles.dividerStyle1}></div>
        <div className={Styles.buttons}>
          <Button
            shape="rectangle"
            justify="center"
            size="small"
            color="secondary"
            onClick={() => {
              props.setOpen(false);
            }}
          >
            Cancel
          </Button>
          <Button
            shape="rectangle"
            justify="center"
            size="small"
            color="primary"
            onClick={() => {
              handleSubmit();
            }}
          >
            Approve for payment
          </Button>
        </div>
      </div>
    </div>
  );
};
export default GrnData;
