import React, { useState, useEffect } from 'react';
import { useParams, useLocation } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import BackArrowIcon from '../menu/icons/backArrow';
import Styles from '../../styles/purchaseView.module.scss';
import { environment } from '../../environment/environment';
import { formatBudgetValue } from '../../helper/common-function';
import CustomLoader from '../ui/customLoader';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import indentApprovalService from '../../service/indent-approval-request-service';
import AddIcon from '../menu/icons/addIcon';
import purchaseRequestService from '../../service/purchaseRequest-service';
import CustomPurchaseRequest from '../ui/CustomPurchaseRequestPopup';
import CustomMenu from '../ui/CustomMenu';
import CustomSnackBar from '../ui/customSnackBar';

const PurchaseView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [tableData, setTableData] = useState([]);
  const [purchaseTableData, setPurchaseTableData] = useState([]);
  const [dataCount, setDataCount] = useState(0);
  const [dataLoading, setDataLoading] = useState(false);
  const [showPurchaseRequestForm, setShowPurchaseRequestForm] = useState(false);
  const [reload, setReload] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  
  const indentId = Number(routeParams?.id);
  const location = useLocation();
  const projectId = location.state.project_id;

  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    indent_request_id: indentId,
  };

  useEffect(() => {
    const getAllData = async () => {
      try {
        setDataLoading(true);
      } finally {
        const result = await indentApprovalService.indentDetailData(masterData);
        if (result.message === 'success') {
          setTableData(result.content);
          setDataLoading(false);
        }
      }
    };
    getAllData();
  }, []);

  const purchaseData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    indent_request_id: indentId,
  };

  useEffect(() => {
    const getAllPurchaseData = async () => {
      const data = await purchaseRequestService.purchaseDetailData(
        purchaseData
      );
      if (data.message === 'success') {
        setPurchaseTableData(data.content);
        setDataCount(data.total_count);
      }
    };
    getAllPurchaseData();
  }, [reload]);

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const nullLableNameFromEnv = `${environment.NULLVALUE}`;

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <div className={Styles.headingTop}>
            <div className={Styles.textContent}>
              <h3>Indent Request Detail List</h3>
              <span className={Styles.content}>
                Manage your Indent raise detail across your project
              </span>
            </div>
            <div className={Styles.backButton}>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                icon={<BackArrowIcon />}
                onClick={() => navigate('/purchase-view')}
              >
                Back
              </Button>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.tableContainer}>
            <div>
              <div className={Styles.tableText}>
                <h3>Indent Detail Table</h3>
              </div>
              <table>
                <thead>
                  <tr>
                    <th>S No</th>
                    <th>Item Name </th>
                    <th>UOM</th>
                    <th>Quantity</th>
                    <th>Total Cost</th>
                  </tr>
                </thead>
                <tbody>
                  {tableData?.map((data: any, index: number) => (
                    <tr key={data.indent_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                      <td>{data?.bom_detail_data?.uom_data?.name}</td>
                      <td>{data?.quantity}</td>
                      <td>{formatBudgetValue(data?.total)}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
          <div className={Styles.approveButton}>
            <div>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                color="primary"
                onClick={() => {navigate('/purchase-request-add',{state:{project_id: projectId,indent_id:indentId}})}}
              >
                <AddIcon color='white' />
                Create PR
              </Button>
            </div>
          </div>
        </div>
      </CustomLoader>
      <div className={Styles.bottomTable}>
        <div className={Styles.tableContainer}>
          <div>
            <div className={Styles.tableText}>
              <h3>Purchase Request Table</h3>
            </div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Purchase Request</th>
                  <th>Vendor Name </th>
                  <th>Budget</th>
                  <th>No of Items</th>
                  <th>Quotation Status</th>
                  <th>Action</th>
                </tr>
              </thead>
              <tbody>
                {dataCount === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    <td></td>
                  </tr>
                ) : (
                  ''
                )}
                {purchaseTableData?.map((data: any, index: number) => {
                  const isStatusApproved = data?.status === 'Approved';
                  const isMarkEnabled = isStatusApproved;
                  const actions = [
                    {
                      label: 'View',
                      onClick: () => {
                        navigate(`/vendor-select/${data?.purchase_request_id}`,{state:{project_id: projectId,indent_id:indentId}});
                      },
                    },
                    {
                      label: 'Move to PO',
                      onClick: () => {
                        if (isMarkEnabled) {
                          navigate(
                            `/purchase-request/${data?.purchase_request_id}`,
                          );
                        }
                      },
                      disabled: !isMarkEnabled,
                    },
                  ];
                  return (
                    <tr key={data.purchase_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>
                        {data.indent_request_data.description ||
                          nullLableNameFromEnv}
                      </td>
                      <td>
                        {data?.selected_vendor_data?.vendor_name ||
                          nullLableNameFromEnv}
                      </td>
                      <td>
                        {data?.total_cost
                          ? formatBudgetValue(data?.total_cost)
                          : nullLableNameFromEnv}
                      </td>
                      <td>
                        {data?.purchase_request_details.length ||
                          nullLableNameFromEnv}
                      </td>
                      <td>{data?.status || 'N.A'}</td>
                      <td>
                        <CustomMenu actions={actions} />
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        </div>
      </div>
      <CustomPurchaseRequest
        isVissible={showPurchaseRequestForm}
        setReload={setReload}
        onAction={setShowPurchaseRequestForm}
        indentId={indentId}
        projectId={projectId}
        setOpenSnack={setOpenSnack}
        setMessage={setMessage}
      ></CustomPurchaseRequest>
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={2000}
        type="success"
      />
    </div>
  );
};

export default PurchaseView;
