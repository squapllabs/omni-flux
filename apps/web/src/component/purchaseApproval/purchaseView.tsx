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
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import CustomGroupButton from '../ui/CustomGroupButton';
import { useGetAllPaginatedPurchaseRequests, getBySearchPurchaseRequestes } from '../../hooks/purchaseRequest-hooks';

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
  const [activeButton, setActiveButton] = useState<string | null>('Evaluating Vendors');
  const [isLoading, setIsLoading] = useState(true);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Evaluating Vendors', value: 'Evaluating Vendors' },
    { label: 'Vendor Shortlisted', value: 'Vendor Shortlisted' },
    { label: 'PO Placed', value: 'PO Placed' },
  ]);
  const [filterValues, setFilterValues] = useState({
    global_search: '',
  });
  const [filter, setFilter] = useState(false);
  const [dataShow, setDataShow] = useState(false);
  const indentId = Number(routeParams?.id);
  const location = useLocation();
  const projectId = location.state.project_id;
  const [isResetDisabled, setIsResetDisabled] = useState(true);

  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    purchase_request_status: activeButton,
    status: 'AC',
    global_search: '',
    indent_request_id: indentId,
  };

  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getBySearchPurchaseRequestes();

  const {
    isLoading: getAllLoadingPurchaseRequestsData,
    data: initialData,
    refetch,
  } = useGetAllPaginatedPurchaseRequests(masterData);

  /* Function for group button (Active and Inactive status) */
  const handleGroupButtonClick = (value: string) => {
    console.log("value", value);
    setActiveButton(value);
  };

  /* Function for Filter Change */
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;

    setFilterValues({
      ...filterValues,
      ['global_search']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      handleReset();
    }
  };



  const handleSearch = async () => {
    const object: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      purchase_request_status: activeButton,
      indent_request_id: indentId,
      ...filterValues,
    };
    postDataForFilter(object);
    setDataShow(true);
    setIsLoading(false);
    setFilter(true);
  };

  /* Function for resting the search field and data to normal state */
  const handleReset = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      purchase_request_status: "Evaluating Vendors",
      project_id: Number(routeParams?.id),
      global_search: '',
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      global_search: '',
    });
    setIsLoading(false);
    setDataShow(false);
    setIsResetDisabled(true);
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

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

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
      <CustomLoader loading={FilterLoading ? FilterLoading : getAllLoadingPurchaseRequestsData} size={48} color="#333C44">
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
                <h3>Indent Detail List</h3>
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
                onClick={() => { navigate('/purchase-request-add', { state: { project_id: projectId, indent_id: indentId } }) }}
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
              <h3>Purchase Request List</h3>
            </div>
            <div className={Styles.searchField}>
              <div className={Styles.inputFilter}>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="global_search"
                  value={filterValues.global_search}
                  onChange={(e) => handleFilterChange(e)}
                  placeholder="Search by Vendor and Status"
                />
                <Button
                  className={Styles.searchButton}
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={handleSearch}
                >
                  Search
                </Button>
                <Button
                  className={Styles.resetButton}
                  shape="rectangle"
                  justify="center"
                  size="small"
                  disabled={isResetDisabled}
                  onClick={handleReset}
                >
                  Reset
                </Button>
              </div>
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
            </div>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Purchase Request</th>
                  <th>Vendor Name </th>
                  <th>Budget</th>
                  <th>No of Items</th>
                  {/* <th>Quotation Status</th> */}
                  <th>Action</th>
                </tr>
              </thead>
              <tbody>
                {dataShow ? (
                  getFilterData?.total_count === 0 ? (
                    <tr>
                      <td colSpan="4" style={{ textAlign: 'center' }}>
                        No data found
                      </td>
                      {activeButton === 'AC' && <td></td>}
                    </tr>
                  ) :
                    (getFilterData?.content?.map((data: any, index: number) => {
                      const isStatusApproved = data?.status === 'Vendor Shortlisted';
                      const isMarkEnabled = isStatusApproved;
                      const actions = [
                        {
                          label: 'View',
                          onClick: () => {
                            navigate(`/vendor-select/${data?.purchase_request_id}`);
                          },
                        },
                        {
                          label: 'Move to PO',
                          onClick: () => {
                            if (isMarkEnabled) {
                              navigate(
                                `/purchase-request/${data?.purchase_request_id}`
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
                          {/* <td>{data?.status || 'N.A'}</td> */}
                          <td>
                            <CustomMenu actions={actions} />
                          </td>
                        </tr>
                      );
                    }))
                ) :
                  initialData?.total_count === 0 ? (
                    <tr>
                      <td colSpan="4" style={{ textAlign: 'center' }}>
                        No data found
                      </td>
                      {/* {activeButton === 'AC' && <td></td>} */}
                    </tr>
                  ) : (initialData?.content?.map((data: any, index: number) => {
                    console.log("sdf",data);
                    const isStatusApproved = data?.status === 'Vendor Shortlisted';
                    const isMarkEnabled = isStatusApproved;
                    const actions = [
                      {
                        label: 'View',
                        onClick: () => {
                          navigate(`/vendor-select/${data?.purchase_request_id}`);
                        },
                      },
                      {
                        label: 'Move to PO',
                        onClick: () => {
                          if (isMarkEnabled) {
                            navigate(
                              `/purchase-request/${data?.purchase_request_id}`
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
                        {/* <td>{data?.status || 'N.A'}</td> */}
                        <td>
                          <CustomMenu actions={actions} />
                        </td>
                      </tr>
                    );
                  }))
                }
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
