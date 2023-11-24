import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  useGetAllPurchaseOrderData,
  useGetBySearchPoData,
} from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import CustomLoader from '../ui/customLoader';
import CustomEditInvoicePopup from '../ui/CustomEditInvoicePopup';
import { formatBudgetValue } from '../../helper/common-function';
import Button from '../ui/Button';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import CustomGroupButton from '../ui/CustomGroupButton';
import ViewIcon from '../menu/icons/newViewIcon';
import ReportGenerator from '../reportGenerator/pdfReport/invoice';
import CustomPagination from '../menu/CustomPagination';
import ProjectSubheader from '../project/projectSubheader';
import { environment } from '../../environment/environment';
import CustomPopup from '../ui/CustomSidePopup';

const OrderView = () => {
  const navigate = useNavigate();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [purchaseId, setPurchaseId] = useState();
  const [selectedValue, setSelectedValue] = useState('');
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const [open, setOpen] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [reload, setReload] = useState(false);
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;
  const [invoiceNumber, setInvoiceNumber] = useState();
  const [message, setMessage] = useState('');
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'To be paid', value: 'Invoice' },
    { label: 'Paid', value: 'Completed' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('Invoice');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const getPoData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    bill_status: activeButton,
    project_id: selectedValue,
    purchase_order_type: 'Head Office',
  };
  /* Function to get all invoice data */
  const {
    isLoading: dataLoading,
    data: getAllData,
    refetch,
  } = useGetAllPurchaseOrderData(getPoData);
  const { data: getAllProjectDataForDrop = [], isLoading: dropLoading } =
    useGetAllProjectDrop();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = useGetBySearchPoData();
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedProjectId = event.target.value;
    setSelectedValue(selectedProjectId);
    setIsResetDisabled(searchValue === '');
  };
  /* Function to search thriugh users input */
  const handleSearch = async () => {
    const poData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'AC',
      global_search: '',
      bill_status: activeButton,
      project_id: Number(selectedValue),
    };
    postDataForFilter(poData);
    setDataShow(true);
  };
  /* Function to reset the search operation */
  const handleReset = async () => {
    const poData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'AC',
      global_search: '',
      bill_status: 'Invoice',
      project_id: Number(selectedValue),
    };
    postDataForFilter(poData);
    setSelectedValue('');
    setDataShow(false);
    setSelectedValue('');
    setIsResetDisabled(true);
  };
  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);
  const handleClosePopup = () => {
    setOpen(false);
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  const generateCustomBillName = (data: any) => {
    if (data) {
      const vendorName = data.vendor_data?.vendor_name || '';
      const projectName =
        data.purchase_request_data?.project_data?.project_name || '';
      const year = new Date().getFullYear();
      const customBillName = `ALM-${projectName.substring(
        0,
        3
      )}-${vendorName.substring(0, 3)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };

  return (
    <div className={Styles.container}>
      <CustomLoader
        loading={searchLoader ? searchLoader : dataLoading}
        size={48}
        color="#333C44"
      >
        <div className={Styles.box}>
          <div>
            <ProjectSubheader
              title="Invoice and Payments"
              description=" Manage payables for the orders"
              navigation={'/home'}
            />
          </div>
          <div className={Styles.dividerStyleTop}></div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div>
                <AutoCompleteSelect
                  name="parent_master_data_id"
                  defaultLabel="Select Project Name"
                  onChange={() => handleDropdownChange}
                  value={selectedValue}
                  placeholder="Select Project Name"
                  width="260px"
                  onSelect={(value) => {
                    setSelectedValue(value);
                    setIsResetDisabled(false);
                  }}
                  optionList={
                    dropLoading === true ? [] : getAllProjectDataForDrop
                  }
                />
              </div>
              <Button
                // className={Styles.searchButton}
                shape="rectangle"
                justify="center"
                size="small"
                color='search'
                onClick={handleSearch}
              >
                Search
              </Button>
              <Button
                // className={Styles.resetButton}
                shape="rectangle"
                justify="center"
                size="small"
                color='reset'
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
        </div>
        <div className={Styles.tableContainer}>
          <div>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th className={Styles.tableHeading}>#</th>
                  <th className={Styles.tableHeading}>Order Id</th>
                  <th className={Styles.tableHeading}>Vendor Name</th>
                  <th className={Styles.tableHeading}>Project Name </th>
                  <th className={Styles.tableHeading}>Amount</th>
                  <th className={Styles.tableHeading}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {dataShow ? (
                  getFilterData?.content?.length > 0 ? (
                    getFilterData?.content?.map((data: any, index: number) => {
                      const customBillName = generateCustomBillName(data);
                      return (
                        <tr>
                          <td>{startingIndex + index}</td>
                          <td>{data?.order_id}</td>
                          <td>{data?.vendor_data?.vendor_name}</td>
                          <td>
                            {
                              data?.purchase_request_data?.project_data
                                ?.project_name
                            }
                          </td>
                          <td>{formatBudgetValue(data?.total_cost)}</td>
                          <td>
                            <td>
                              <ViewIcon
                                onClick={
                                  () =>
                                    navigate(
                                      `/view-invoice/${data.purchase_order_id}`
                                    )
                                  // navigate(`/invoice-view/${data.purchase_order_id}`)
                                }
                              />
                            </td>
                          </td>
                        </tr>
                      );
                    })
                  ) : (
                    <tr>
                      <td colSpan="6" style={{ textAlign: 'center' }}>
                        No data found
                      </td>
                    </tr>
                  )
                ) : getAllData?.content?.length > 0 ? (
                  getAllData?.content?.map((data: any, index: number) => {
                    const customBillName = generateCustomBillName(data);
                    return (
                      <tr>
                        <td>{startingIndex + index}</td>
                        <td>{data?.order_id}</td>
                        <td>
                          {data?.vendor_data?.vendor_name ||
                            nullLableNameFromEnv}
                        </td>
                        <td>
                          {data?.purchase_request_data?.project_data
                            ?.project_name || nullLableNameFromEnv}
                        </td>
                        <td>{formatBudgetValue(data?.total_cost)}</td>
                        <td>
                          <ViewIcon
                            onClick={
                              () =>
                                navigate(
                                  `/view-invoice/${data.purchase_order_id}`
                                )
                              // navigate(`/invoice-view/${data.purchase_order_id}`)
                            }
                          />
                        </td>
                      </tr>
                    );
                  })
                ) : (
                  <tr>
                    <td colSpan="6" style={{ textAlign: 'center' }}>
                      No data found
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
        <div className={Styles.pagination}>
          <CustomPagination
            currentPage={currentPage}
            totalPages={
              dataShow ? getFilterData?.total_page : getAllData?.total_page
            }
            totalCount={
              dataShow ? getFilterData?.total_count : getAllData?.total_count
            }
            rowsPerPage={rowsPerPage}
            onPageChange={handlePageChange}
            onRowsPerPageChange={handleRowsPerPageChange}
          />
        </div>
      </CustomLoader>
      <CustomPopup
        title="Edit Payment Details"
        open={open}
        handleClose={handleClosePopup}
        content={
          <CustomEditInvoicePopup
            setOpen={setOpen}
            open={open}
            setReload={setReload}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
            selectedPurchaseOrder={purchaseId}
            selectedInvoive={invoiceNumber}
          />
        }
      />
    </div>
  );
};

export default OrderView;
