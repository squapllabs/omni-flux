import React from 'react';
import { useGetByStockAuditId } from '../../../hooks/stockAudit-hooks';
import { useParams, useNavigate } from 'react-router-dom';
import Styles from '../../../styles/project.module.scss';
import CustomLoader from '../../ui/customLoader';
import PreviousPageIcon from '../../menu/icons/previousPageIcon';
/* Function to view stock audit details */
const ProjectStockAuditView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  /* Function to get stock data */
  const { data: getStockData, isLoading: dataLoading } = useGetByStockAuditId(
    Number(routeParams?.id)
  );

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.sub_header}>
          <div
            className={Styles.logo}
            onClick={() => {
              navigate(`/project-edit/${getStockData?.project_id}`);
            }}
          >
            <PreviousPageIcon width={20} height={20} color="#7f56d9" />
          </div>
          <div style={{ padding: '8px', display: 'flex' }}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              width: '700px',
            }}
          >
            <div className={Styles.content_container_left}>
              <span>Project Name :</span>
              <span>Site Name :</span>
            </div>
            <div className={Styles.content_container}>
              <span>{getStockData?.project_data?.project_name}</span>
              <span>{getStockData?.site_data?.name}</span>
            </div>
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.box}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>S.No</th>
                <th className={Styles.tableHeading}>Item</th>
                <th className={Styles.tableHeading}>Quantity</th>
              </tr>
            </thead>
            <tbody>
              {getStockData?.item_details?.map((items: any, index: any) => {
                rowIndex = rowIndex + 1;
                return (
                  <tr>
                    <td>{rowIndex}</td>
                    <td>{items?.item_name}</td>
                    <td>{items?.quantity}</td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </CustomLoader>
    </div>
  );
};

export default ProjectStockAuditView;
