import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  useGetAllPurchaseOrderData,
  getBySearchPoData,
} from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import CustomLoader from '../ui/customLoader';
import EditIcon from '../menu/icons/newEditIcon';
import PdfDownloadIcon from '../menu/icons/pdfDownloadIcon';
import CustomEditPoPopup from '../ui/CustomEditPoPopup';
import { formatBudgetValue } from '../../helper/common-function';
// import Pagination from '../menu/pagination';
import CustomPagination from '../menu/CustomPagination';
import Button from '../ui/Button';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllProject } from '../../hooks/project-hooks';
import ReportGenerator from '../ui/reportGenerator';
import AddIcon from '../menu/icons/addIcon';
import PurchaseOrderReport from '../reportGenerator/pdfReport/purchaseOrder';
import ProjectSubheader from '../project/projectSubheader';
import ViewIcon from '../menu/icons/newViewIcon';
import PurchaseOrderExcelReport from '../reportGenerator/excelReport/purchaseOrder'
import ExcelIcon from '../menu/icons/excelIcon';

const OrderView = () => {
  const navigate = useNavigate();
  const [showEditPopUp, setShowEditPopUp] = useState(false);
  const [pdfDownload, setPdfDownload] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [purchaseId, setPurchaseId] = useState();
  const [selectedValue, setSelectedValue] = useState('');
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const getPoData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search: '',
    project_id: selectedValue,
  };
  const {
    isLoading: dataLoading,
    data: getAllData,
    refetch,
  } = useGetAllPurchaseOrderData(getPoData);
  const { data: getAllProjectDataForDrop = [], isLoading: dropLoading } =
    useGetAllProject();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getBySearchPoData();

  const handleEdit = (value: any) => {
    setPurchaseId(value);
    setShowEditPopUp(true);
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

  const handleReportGenerator = async (data: any) => {
    await PurchaseOrderReport(data);
  };
  const handleExcelReportGenerator = async (data: any) => {
    await PurchaseOrderExcelReport(data);
  };

  const handleSearch = async () => {
    const poData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'AC',
      global_search: '',
      project_id: Number(selectedValue),
    };
    postDataForFilter(poData);
    setDataShow(true);
  };

  const handleReset = async () => {
    setSelectedValue('');
    setDataShow(false);
    setSelectedValue('');
    setIsResetDisabled(true);
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage]);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  const generateCustomQuotationName = (data: any) => {
    if (data) {
      const vendorName = data.vendor_data?.vendor_name || '';
      const year = new Date().getFullYear();
      const customBillName = `ALM-${vendorName.substring(0, 5)}-${year}`;
      return customBillName.toUpperCase();
    }
    return '';
  };
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
              navigation={'/home'}
              description="Overview of all purchase orders within your organization"
              title="Check PO Progress"
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
          </div>
        </div>
        <div className={Styles.tableContainer}>
          <div>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th className={Styles.tableHeading}>#</th>
                  <th className={Styles.tableHeading}>Vendor Name</th>
                  <th className={Styles.tableHeading}>Project Name </th>
                  <th className={Styles.tableHeading}>Amount</th>
                  <th className={Styles.tableHeading}>Quotation </th>
                  <th className={Styles.tableHeading}>Status</th>
                  <th className={Styles.tableHeading}>Bill</th>
                  <th className={Styles.tableHeading}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {dataShow
                  ? getFilterData?.content?.map((data: any, index: number) => {
                      const customBillName = generateCustomBillName(data);
                      const customQuotationName =
                        generateCustomQuotationName(data);
                      return (
                        <tr>
                          <td>{startingIndex + index}</td>
                          <td>{data?.vendor_data?.vendor_name}</td>
                          <td>
                            {
                              data?.purchase_request_data?.project_data
                                ?.project_name
                            }
                          </td>
                          <td>{formatBudgetValue(data?.total_cost)}</td>
                          <td>
                            <div>
                              {data?.purchase_request_data
                                ?.purchase_request_documents?.length > 0 ? (
                                data?.purchase_request_data?.purchase_request_documents.map(
                                  (document: any, index: number) => (
                                    <div key={document.code}>
                                      <a
                                        href={document.path}
                                        target="_blank"
                                        rel="noopener noreferrer"
                                      >
                                        {customQuotationName}
                                        {/* Uploaded Document */}
                                      </a>
                                    </div>
                                  )
                                )
                              ) : (
                                <div>-</div>
                              )}
                            </div>
                          </td>
                          <td>{data.status}</td>
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
                                        {/* Uploaded Document */}
                                      </a>
                                    </div>
                                  )
                                )
                              ) : (
                                <div>-</div>
                              )}
                            </div>
                          </td>
                          <td>
                            <div className={Styles.tablerow}>
                              <EditIcon
                                onClick={() =>
                                  handleEdit(data.purchase_order_id)
                                }
                              />
                              <PdfDownloadIcon
                                onClick={() => handleReportGenerator(data)}
                              />
                              <ViewIcon
                                onClick={() => 
                                  navigate(`/purchase-order-view/${data.purchase_order_id}`)}
                              />
                            </div>
                          </td>
                        </tr>
                      );
                    })
                  : getAllData?.content?.map((data: any, index: number) => {
                      const customBillName = generateCustomBillName(data);
                      const customQuotationName =
                        generateCustomQuotationName(data);
                      return (
                        <tr>
                          <td>{startingIndex + index}</td>
                          <td>{data?.vendor_data?.vendor_name}</td>
                          <td>
                            {
                              data?.purchase_request_data?.project_data
                                ?.project_name
                            }
                          </td>
                          <td>{formatBudgetValue(data?.total_cost)}</td>
                          <td>
                            <div>
                              {data?.purchase_request_data
                                ?.purchase_request_documents?.length > 0 ? (
                                data?.purchase_request_data?.purchase_request_documents.map(
                                  (document: any, index: number) => (
                                    <div key={document.code}>
                                      <a
                                        href={document.path}
                                        target="_blank"
                                        rel="noopener noreferrer"
                                      >
                                        {customQuotationName}
                                        {/* Uploaded Document */}
                                      </a>
                                    </div>
                                  )
                                )
                              ) : (
                                <div>-</div>
                              )}
                            </div>
                          </td>
                          <td>{data.status}</td>
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
                                        {/* Uploaded Document */}
                                      </a>
                                    </div>
                                  )
                                )
                              ) : (
                                <div>-</div>
                              )}
                            </div>
                          </td>
                          <td>
                            <div className={Styles.tablerow}>
                              <EditIcon
                                onClick={() =>
                                  handleEdit(Number(data.purchase_order_id))
                                }
                              />
                              <ViewIcon
                                onClick={() => 
                                  navigate(`/purchase-order-view/${data.purchase_order_id}`)}
                              />
                               <PdfDownloadIcon
                                onClick={() => handleReportGenerator(data)}
                              />
                              {/* <ExcelIcon  onClick={() => handleExcelReportGenerator(data)}/> */}
                            </div>
                          </td>
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
      <CustomEditPoPopup
        isVissible={showEditPopUp}
        onAction={setShowEditPopUp}
        selectedPurchaseOrder={purchaseId}
      />
      {pdfDownload ? <ReportGenerator /> : ''}
    </div>
  );
};

export default OrderView;
