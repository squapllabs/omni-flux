import React, { useState, useEffect } from 'react';
import { useParams, useLocation } from 'react-router-dom';
import { useNavigate } from 'react-router-dom';
import Button from '../ui/Button';
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
import ProjectSubheader from '../project/projectSubheader';
import PdfDownloadIcon from '../menu/icons/pdfDownloadIcon';
import ReportGenerator from '../reportGenerator/pdfReport/requestForQuotation';
import Checkbox from '../ui/Checkbox';

const PurchaseView = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(50);
  const [tableData, setTableData] = useState([]);
  const [selectedRows, setSelectedRows] = useState([]);
  const [purchaseTableData, setPurchaseTableData] = useState([]);
  const [dataCount, setDataCount] = useState(0);
  const [dataLoading, setDataLoading] = useState(false);
  const [showPurchaseRequestForm, setShowPurchaseRequestForm] = useState(false);
  const [reload, setReload] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [selectAllChecked, setSelectAllChecked] = useState(false);
  const [selectedRowCount, setSelectedRowCount] = useState(0);

  const indentId = Number(routeParams?.id);
  const location = useLocation();
  const projectId = location.state.project_id;

  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    indent_request_id: indentId,
  };

  const handleReportGenerator = (data: any) => {
    ReportGenerator(data);
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
    order_by_column: 'created_date',
    order_by_direction: 'asc',
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
  const handleSelectAllChange = () => {
    if (!selectAllChecked) {
      setSelectedRows(tableData);
      setSelectedRowCount(tableData?.length); // Select all rows
    } else {
      setSelectedRows([]);
      setSelectedRowCount(0); // Deselect all rows
    }
    setSelectAllChecked(!selectAllChecked);
  };
  const handleCheckboxChange = (e: any, rowData: any) => {
    if (e.target.checked) {
      setSelectedRows([...selectedRows, rowData]);
    } else {
      setSelectedRows(selectedRows.filter((row) => row !== rowData));
    }
    setSelectedRowCount(selectedRows.length);
  };

  useEffect(() => {
    setSelectedRowCount(selectedRows.length);
  }, [selectedRows]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <ProjectSubheader
            description="Manage your Indent raise detail across your project"
            navigation={'/approved-indent-list'}
            title="Indent Request Detail List"
          />
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.topHeading}>
            <div className={Styles.tableTopData}>
              <div className={Styles.rowCount}>
                <span>Selected Items ({selectedRowCount})</span>
              </div>
              <div>
                <Button
                  shape="rectangle"
                  justify="center"
                  size="small"
                  color="primary"
                  onClick={() => {
                    navigate('/vendor-select', {
                      state: {
                        tableData: selectedRows,
                        indentId: indentId,
                        projectId: projectId,
                      },
                    });
                  }}
                >
                  Select vendor to get Quote
                </Button>
              </div>
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th className={Styles.tableHeadingOne}>
                    <Checkbox
                      name="select-all"
                      checked={selectAllChecked}
                      onChange={() => handleSelectAllChange()}
                    />
                  </th>
                  <th className={Styles.tableHeadingTwo}>#</th>
                  <th className={Styles.tableHeadingThree}>Item Name </th>
                  <th className={Styles.tableHeadingFour}>UOM</th>
                  <th className={Styles.tableHeadingFive}>Quantity</th>
                  {/* <th className={Styles.tableHeading}>Unit Cost</th>
                    <th className={Styles.tableHeading}>Total Cost</th> */}
                </tr>
              </thead>
              <tbody>
                {tableData?.map((data: any, index: number) => (
                  <tr key={data.indent_request_details_id}>
                    <td>
                      <Checkbox
                        name="is_remember_me"
                        checked={selectedRows.includes(data)}
                        onChange={(e) => handleCheckboxChange(e, data)}
                      />
                    </td>
                    <td>{startingIndex + index}</td>
                    <td>{data?.bom_detail_data?.item_data?.item_name}</td>
                    <td>{data?.bom_detail_data?.uom_data?.name}</td>
                    <td>{data?.indent_requested_quantity}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </CustomLoader>
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
