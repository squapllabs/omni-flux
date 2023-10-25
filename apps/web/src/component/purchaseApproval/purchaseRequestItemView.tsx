import { useNavigate, useLocation } from 'react-router-dom';
import { useParams } from 'react-router-dom';
import Styles from '../../styles/vendorSelect.module.scss';
import PreviousPageIcon from '../menu/icons/previousPageIcon';

import { environment } from '../../environment/environment';
const VendorSelect = () => {
  const routeParams = useParams();
  const location = useLocation();
  const indentId = location.state.indent_id;
  const projectId = location.state.project_id;
  const tableData = location.state.data;
  const pageName = location.state.page;
  const prId = location.state.purchaseRequestId
  const navigate = useNavigate();
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;
  return (
    <div className={Styles.container}>
      <div className={Styles.sub_header}>
        <div
          className={Styles.logo}
          onClick={() => {
            if (pageName === 'VendorPage') {
             navigate(`/vendor-select/${prId}`,{state:{project_id: projectId,indent_id:indentId}})
            } else {
              navigate(`/purchase-detail/${indentId}`, {
                state: { project_id: projectId },
              });
            }
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
          <div className={Styles.textContent_1}>
            <h3>Items List</h3>
            <span className={Styles.content}>View the requested items</span>
          </div>
        </div>
      </div>
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.tableContainer}>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th>S No</th>
              <th>Item Name</th>
              <th>Requested Quantity</th>
              <th>Allocated Quantity</th>
            </tr>
          </thead>
          <tbody>
            {tableData?.length === 0 ? (
              <tr>
                <td colspan="4" style={{ textAlign: 'center' }}>
                  No data found
                </td>
              </tr>
            ) : (
              tableData?.map((item: any, index: any) => {
                
                return (
                  <tr>
                    <td>{index + 1}</td>
                    <td>{item.item_name}</td>
                    <td>{item.indent_requested_quantity || nullLableNameFromEnv}</td>
                    <td>{item.purchase_requested_quantity || nullLableNameFromEnv}</td>
                  </tr>
                );
              })
            )}
          </tbody>
        </table>
      </div>
    </div>
  );
};
export default VendorSelect;
