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
import ProjectSubheader from '../../projectSubheader';
import SiteNavigateIcon from '../../../menu/icons/siteNavigateIcon';
import PersonIcon from '../../../menu/icons/personIcon';

const LocalPurchaseOrder = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const location = useLocation();
  const projectId = location.state.project_id;
  const indentId = Number(routeParams?.id);
  const [indentRequestData, setIndentRequestData] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(50);

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

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div>
      <div>
        <ProjectSubheader
          title="Local Purchase Order"
          navigation={`/project-edit/${projectId}`}
          description=""
        />
      </div>
      <div className={Styles.sub_header}>
        <div style={{ display: 'flex' }}>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              padding: '20px 10px 20px 20px',
            }}
          >
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Expense Code</span>
              <h3>{indentRequestData?.data?.indent_request_code}</h3>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              padding: '20px 10px 20px 20px',
            }}
          >
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Project</span>
              <h3>{indentRequestData?.data?.project_data?.project_name}</h3>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div style={{ display: 'flex', alignItems: 'center', gap: '20px' }}>
            <div>
              <SiteNavigateIcon width={30} height={30} />
            </div>
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Site </span>
              <h3>{indentRequestData?.data?.site_data?.name}</h3>
            </div>
          </div>
        </div>
        <div className={Styles.boqAmount}>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: '10px',
              padding: '20px 10px 20px 10px',
            }}
          >
            <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  indentRequestData?.data?.total_cost
                    ? indentRequestData?.data?.total_cost
                    : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Total Cost</span>
            </div>
          </div>
        </div>
      </div>
      <div className={Styles.selected}></div>
      <div>
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>#</th>
                <th className={Styles.tableHeading}>Item Name </th>
                <th className={Styles.tableHeading}>UOM</th>
                <th className={Styles.tableHeading}>Quantity</th>
                <th className={Styles.tableHeading}>Cost</th>
              </tr>
            </thead>
            <tbody>
              {indentRequestData?.data?.indent_request_details?.map(
                (data: any, index: any) => {
                  return (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                      <td>{data?.bom_detail_data?.uom_data?.name}</td>
                      <td>{data?.indent_requested_quantity}</td>
                      <td>{formatBudgetValue(data?.total)}</td>
                    </tr>
                  );
                }
              )}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
};
export default LocalPurchaseOrder;
