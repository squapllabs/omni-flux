import React, { useState, useEffect } from 'react';
import { useParams, useLocation } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
import BackArrowIcon from '../menu/icons/backArrow';
import Styles from '../../styles/purchaseView.module.scss';
import { formatBudgetValue } from '../../helper/common-function';
import CustomLoader from '../ui/customLoader';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import indentApprovalService from '../../service/indent-approval-request-service';
import AddIcon from '../menu/icons/addIcon';
import purchaseRequestService from '../../service/purchaseRequest-service';
import EditIcon from '../menu/icons/editIcon';
import CustomEditDialog from '../ui/customEditDialogBox';
import CustomPurchaseRequest from '../ui/CustomPurchaseRequestPopup';
import PurchaseRequestEdit from './purchaseRequestEdit';
import ViewIcon from '../menu/icons/viewIcon';

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
  const [Id, setID] = useState();
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [onAction, setOnAction] = useState(false);
  const [showPurchaseRequestForm, setShowPurchaseRequestForm] = useState(false);
  const [reload, setReload] = useState(false);

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
      console.log('pdata', data);

      if (data.message === 'success') {
        setPurchaseTableData(data.content);
        setDataCount(data.total_count);
      }
    };
    getAllPurchaseData();
  }, [reload]);

  const handleEdit = (value: any) => {
    setMode('EDIT');
    setID(value);
    setOpen(true);
  };

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
                  {tableData?.map((data: any, index: number) => {
                    return (
                      <tr key={data.indent_request_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                        <td>{data?.bom_detail_data?.uom_data?.name}</td>
                        <td>{data?.quantity}</td>
                        <td>{formatBudgetValue(data?.total)}</td>
                      </tr>
                    );
                  })}
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
                onClick={() => setShowPurchaseRequestForm(true)}
              >
                <AddIcon />
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
                  {/* <th>Budget</th> */}
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
                  return (
                    <tr key={data.purchase_request_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data.indent_request_data.description}</td>
                      <td>{data?.selected_vendor_data?.vendor_name}</td>
                      {/* <td>{formatBudgetValue(data?.total_cost)}</td> */}
                      <td></td>
                      <td>{data?.status}</td>
                      <td>
                        {
                          // <EditIcon onClick={() => handleEdit(data.purchase_request_id)}/>
                          <ViewIcon
                            onClick={() =>
                              navigate(
                                `/vendor-select/${data?.purchase_request_id}`
                              )
                            }
                          />
                        }
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
      ></CustomPurchaseRequest>
      <CustomEditDialog
        open={open}
        content={
          <PurchaseRequestEdit
            setOpen={setOpen}
            open={open}
            mode={mode}
            purchaseID={Id}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
      />
    </div>
  );
};

export default PurchaseView;
