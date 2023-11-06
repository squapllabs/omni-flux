import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  useGetAllPurchaseOrderData,
  getBySearchPoData,
} from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import CustomLoader from '../ui/customLoader';
import EditIcon from '../menu/icons/newEditIcon';
import CustomEditInvoicePopup from '../ui/CustomEditInvoicePopup';
import { formatBudgetValue } from '../../helper/common-function';
import Pagination from '../menu/pagination';
import Button from '../ui/Button';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import CustomGroupButton from '../ui/CustomGroupButton';
import ViewIcon from '../menu/icons/newViewIcon';
import { format } from 'date-fns';
import PdfDownloadIcon from '../menu/icons/pdfDownloadIcon';
import ReportGenerator from '../reportGenerator/pdfReport/invoice';
import CustomPagination from '../menu/CustomPagination';
import ProjectSubheader from '../project/projectSubheader';
import { environment } from '../../environment/environment';
import CustomPopup from '../ui/CustomSidePopup';

const OrderView = () => {
  const navigate = useNavigate();
  const [showEditPopUp, setShowEditPopUp] = useState(false);
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
  };
  const {
    isLoading: dataLoading,
    data: getAllData,
    refetch,
  } = useGetAllPurchaseOrderData(getPoData);
  console.log('ttttttt', getAllData);
  const { data: getAllProjectDataForDrop = [], isLoading: dropLoading } =
    useGetAllProjectDrop();
  // console.log("getAllProjectDataForDrop",getAllProjectDataForDrop);

  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getBySearchPoData();
  console.log('rrrr', getFilterData);

  const handleEdit = (value: any, invoice: any) => {
    setPurchaseId(value);
    setInvoiceNumber(invoice);
    // setShowEditPopUp(true);
    setOpen(true);
    // alert(invoice)
  };

  const handleReportGenerator = () => {
    const data: any = {
      title: 'Invoice and Payments',
      name: 'invoice',
    };
    ReportGenerator(data);
  };

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
                  {activeButton === 'Completed' && <th>Payment Date</th>}
                  {activeButton === 'Completed' && <th>Payment Mode</th>}
                  <th className={Styles.tableHeading}>Invoice</th>
                  <th className={Styles.tableHeading}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {dataShow
                  ? getFilterData?.content?.map((data: any, index: number) => {
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
                          {activeButton === 'Completed' && (
                            <td>
                              {data?.payment_date
                                ? `${format(
                                    new Date(data?.payment_date),
                                    'MMM dd, yyyy'
                                  )}`
                                : nullLableNameFromEnv}
                            </td>
                          )}
                          {activeButton === 'Completed' && (
                            <td>
                              {data?.payment_mode !== 'null'
                                ? data?.payment_mode
                                : nullLableNameFromEnv}
                            </td>
                          )}
                          <td>
                            <div>
                              {data?.purchase_order_documents?.length > 0 ? (
                                data?.purchase_order_documents.map(
                                  (document: any, index: number) => (
                                    <div key={document.code}>
                                      <a
                                        href={document.path}
                                        target="_blank"
                                        rel="noopener noreferrer"
                                      >
                                        {customBillName}
                                      </a>
                                    </div>
                                  )
                                )
                              ) : (
                                <div>-</div>
                              )}
                            </div>
                          </td>
                          {activeButton === 'Invoice' ? (
                            <td>
                              <div className={Styles.tablerow}>
                                {/* <EditIcon
                                  onClick={() =>
                                    handleEdit(data.purchase_order_id)
                                  }
                                /> */}
                                <PdfDownloadIcon
                                  onClick={() => handleReportGenerator()}
                                />
                              </div>
                            </td>
                          ) : (
                            <td>
                              <div className={Styles.tablerow}>
                                {/* <ViewIcon
                                  onClick={() =>
                                    navigate(
                                      `/invoice-view/${data.purchase_order_id}`
                                    )
                                  }
                                /> */}
                                <PdfDownloadIcon
                                  onClick={() => handleReportGenerator()}
                                />
                              </div>
                            </td>
                          )}
                        </tr>
                      );
                    })
                  : getAllData?.content?.map((data: any, index: number) => {
                      const customBillName = generateCustomBillName(data);
                      return (
                        <tr>
                          <td>{startingIndex + index}</td>
                          <td>{data?.order_id}</td>
                          <td>{data?.vendor_data?.vendor_name || nullLableNameFromEnv}</td>
                          <td>
                            {
                              data?.purchase_request_data?.project_data
                                ?.project_name || nullLableNameFromEnv
                            }
                          </td>
                          <td>{formatBudgetValue(data?.total_cost)}</td>
                          {activeButton === 'Completed' && (
                            <td>
                              {data?.payment_date
                                ? `${format(
                                    new Date(data?.payment_date),
                                    'MMM dd, yyyy'
                                  )}`
                                : nullLableNameFromEnv}
                            </td>
                          )}
                          {activeButton === 'Completed' && (
                            <td>
                              {data?.payment_mode || nullLableNameFromEnv}{' '}
                            </td>
                          )}
                          <td>
                            <div>
                              {data?.grn?.length > 0 ? (
                                data?.grn.map(
                                  (document: any, index: number) => (
                                    <div key={document.code}>
                                      <a
                                        href={document.bill_details[0].path}
                                        target="_blank"
                                        rel="noopener noreferrer"
                                      >
                                        {customBillName}
                                      </a>
                                    </div>
                                  )
                                )
                              ) : (
                                <div>-</div>
                              )}
                            </div>
                          </td>
                          {activeButton === 'Invoice' ? (
                            <td>
                              <div className={Styles.tablerow}>
                                <ViewIcon
                                  onClick={
                                    () => navigate(`/view-invoice`)
                                    // navigate(`/invoice-view/${data.purchase_order_id}`)
                                  }
                                />
                                <EditIcon
                                  onClick={() =>
                                    handleEdit(
                                      data.purchase_order_id,
                                      data?.grn[0]?.invoice_id
                                    )
                                  }
                                />
                                <PdfDownloadIcon
                                  onClick={() => handleReportGenerator()}
                                />
                              </div>
                            </td>
                          ) : (
                            <td>
                              <div className={Styles.tablerow}>
                                {/* <ViewIcon
                                  onClick={() =>
                                    navigate(
                                      `/invoice-view/${data.purchase_order_id}`
                                    )
                                  }
                                /> */}
                                <PdfDownloadIcon
                                  onClick={() => handleReportGenerator()}
                                />
                              </div>
                            </td>
                          )}
                        </tr>
                      );
                    })}
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
      {/* <CustomEditInvoicePopup
        isVissible={showEditPopUp}
        onAction={setShowEditPopUp}
        selectedPurchaseOrder={purchaseId}
      /> */}
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
