import React from 'react';
import { getByStockAuditId } from '../../../hooks/stockAudit-hooks';
import { useParams } from 'react-router-dom';
import Styles from '../../../styles/project.module.scss';
import { format } from 'date-fns';
import CustomLoader from '../../ui/customLoader';

const ProjectStockAuditView = () => {
  const routeParams = useParams();
  let rowIndex = 0;
  // console.log('routeParams?.id', routeParams?.id);
  const { data: getStockData, isLoading: dataLoading } = getByStockAuditId(
    Number(routeParams?.id)
  );
  // console.log('getStockData==>', getStockData);

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <div
            style={{
              width: '30%',
              display: 'flex',
              gap: '10px',
              paddingBottom: '10px',
            }}
          >
            <div className={Styles.content_container_left}>
              <span>Project Name :</span>
              <span>Site Name :</span>
              {/* <span>Audit Date :</span> */}
            </div>
            <div className={Styles.content_container}>
              <span>{getStockData?.project_data?.project_name}</span>
              <span>{getStockData?.site_data?.name}</span>
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S.No</th>
                <th>Item</th>
                <th>Quantity</th>
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
