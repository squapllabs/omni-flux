import React, { useState, useEffect } from 'react';
import PreviousPageIcon from '../../../menu/icons/previousPageIcon';
import { useParams, useNavigate, useLocation } from 'react-router-dom';
import CustomLoader from '../../../ui/customLoader';
import { environment } from '../../../../environment/environment';
import { formatBudgetValue } from '../../../../helper/common-function';
import { format } from 'date-fns';
import Styles from '../../../../styles/newStyles/localPurchase.module.scss';
import { useGetAllIndentRequestDetail } from '../../../../hooks/indent-approval-hooks';
import indentApprovalRequestService from '../../../../service/indent-approval-request-service';

const LocalPurchaseOrder = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const location = useLocation();
  const projectId = location.state.project_id;
  const indentId = Number(routeParams?.id);
  const [indentRequestData, setIndentRequestData] = useState('');

  // const indentData = {
  //   limit: rowsPerPage,
  //   offset: (currentPage - 1) * rowsPerPage,
  //   order_by_column: 'created_date',
  //   order_by_direction: 'desc',
  //   status: 'AC',
  //   global_search: '',
  //   indent_request_id: indentId,
  // };

  // const {
  //   data: getAllData,
  //   isLoading: dataLoading,
  //   refetch,
  // } = useGetAllIndentRequestDetail(indentData);
  // console.log('getAllData', getAllData);

  const fetchData = async () => {
    const indentData = await indentApprovalRequestService.getOneIndentById(
      indentId
    );
    console.log('indentData', indentData);
    setIndentRequestData(indentData);
  };

  useEffect(() => {
    fetchData();
  }, [indentId]);

  let rowIndex = 0;

  return (
    <div className={Styles.container}>
      <div className={Styles.sub_header}>
        <div
          className={Styles.logo}
          onClick={() => {
            navigate(`/project-edit/${projectId}`);
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
            <span>{indentRequestData?.data?.project_data?.project_name}</span>
            <span>{indentRequestData?.data?.site_data?.name}</span>
          </div>
        </div>
      </div>
      <div className={Styles.dividerStyle}></div>
      {/* main data */}
      <div className={Styles.secondData}>
        
      </div>
    </div>
  );
};
export default LocalPurchaseOrder;
