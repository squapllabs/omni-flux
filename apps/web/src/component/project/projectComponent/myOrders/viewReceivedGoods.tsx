import React from 'react';
import Styles from '../../../../styles/viewReceivedGoods.module.scss';
import { useNavigate, useParams, useLocation } from 'react-router-dom';
import PreviousPageIcon from '../../../menu/icons/previousPageIcon';
import { useGetOneGrnById } from '../../../../hooks/grn-hooks';
import CustomLoader from '../../../ui/customLoader';

const ViewReceivedGoods = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const { state } = useLocation();
  const projectId = state?.projectId;
  /* Function to get grn data based on grn Id */
  const { data: getListData, isLoading: dataLoading } = useGetOneGrnById(
    Number(routeParams?.grnId)
  );

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
                  <h4>{getListData?.project_data?.project_name}</h4>
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
                    <th>Received Quantity</th>
                  </tr>
                </thead>
                <tbody>
                  {getListData?.grn_details?.map((data: any, index: any) => {
                    return (
                      <tr>
                        <td>{index + 1}</td>
                        <td>{data?.item_data?.item_name}</td>
                        <td>{data?.received_quantity ? data?.received_quantity : 0}</td>
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
