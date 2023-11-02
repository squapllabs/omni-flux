import React, { useState, useEffect } from 'react';
import Styles from '../../../../styles/viewReceivedGoods.module.scss';
import { useNavigate, useParams, useLocation } from 'react-router-dom';
import PreviousPageIcon from '../../../menu/icons/previousPageIcon';
// import grnService from 'apps/web/src/service/grn-service';
import { useGetOneGrnById } from '../../../../hooks/grn-hooks';
import { format } from 'date-fns';
import CustomLoader from '../../../ui/customLoader';
import { environment } from '../../../../environment/environment';

const ViewReceivedGoods = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const grn_Id = Number(routeParams?.grnId);
  const PurchaseOrderId = Number(routeParams?.pruchaseId);
  const { state } = useLocation();
  const projectId = state?.projectId;
  const [initialData, setInitialData] = useState();
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;
  const { data: getListData, isLoading: dataLoading } = useGetOneGrnById(
    Number(routeParams?.grnId)
  );
  console.log('kkkkkkk', getListData);

  // useEffect(() => {
  //     const fetchData = async () => {
  //         const data = await grnService.getGrnById(grn_Id);
  //         setInitialData(data?.data)
  //     };
  //     if (grn_Id) fetchData();
  // }, [])

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd-MM-yyyy');
    return formattedDate;
  };

  return (
    <div>
      <CustomLoader loading={dataLoading} size={48}>
        <div className={Styles.container}>
          <div className={Styles.sub_header}>
            <div
              className={Styles.logo}
              onClick={() => {
                navigate(`/project-edit/${Number(projectId)}`, {
                  state: { projectId },
                });
              }}
            >
              <PreviousPageIcon width={20} height={20} color="#7f56d9" />
            </div>
            <div style={{ display: 'flex' }}>
              <div className={Styles.vertical}>
                <div className={Styles.verticalLine}></div>
              </div>
            </div>
            <div className={Styles.orderDetails}>
              <div className={Styles.leftOrderDetail}>
                <span>
                  <h4>Received Goods</h4>
                </span>
                <span>
                  <p className={Styles.description}>
                    View the Received Goods details
                  </p>
                </span>
              </div>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div>
            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>S No</th>
                    <th>Item Name</th>
                    <th>Description</th>
                    <th>Received Quantity</th>
                    {/* <th>Accepted Quantity</th> */}
                    {/* <th>Options</th> */}
                  </tr>
                </thead>
                <tbody>
                  {getListData?.grn_details?.map((data: any, index: any) => {                    
                    return (
                      <tr>
                        <td>{index + 1}</td>
                        <td>{data?.item_data?.item_name}</td>
                        <td>{data?.item_data?.description}</td>
                        <td>{data?.received_quantity ? data?.received_quantity : 0}</td>
                        {/* <td>{data?.accepted_quantity || nullLableNameFromEnv}</td> */}
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default ViewReceivedGoods;
