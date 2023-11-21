import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  useGetAllPurchaseOrderData,
  usePurchaseOrderGetAll,
} from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import CustomLoader from '../ui/customLoader';
import PdfDownloadIcon from '../menu/icons/pdfDownloadIcon';
import CustomEditPoPopup from '../ui/CustomEditPoPopup';
import { formatBudgetValue } from '../../helper/common-function';
import CustomPagination from '../menu/CustomPagination';
import Button from '../ui/Button';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllProject } from '../../hooks/project-hooks';
import ReportGenerator from '../ui/reportGenerator';
import PurchaseOrderReport from '../reportGenerator/pdfReport/purchaseOrder';
import ProjectSubheader from '../project/projectSubheader';
import ViewIcon from '../menu/icons/newViewIcon';
import PurchaseOrderExcelReport from '../reportGenerator/excelReport/purchaseOrder';
import PrintIcon from '../menu/icons/printIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
/* Purchase order view screen */
const OrderView = () => {
  const navigate = useNavigate();
  const [showEditPopUp, setShowEditPopUp] = useState(false);
  const [pdfDownload, setPdfDownload] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [purchaseId, setPurchaseId] = useState();
  const [selectedValue, setSelectedValue] = useState('');
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'PO Released', value: 'Processing' },
    { label: 'Partially Recieved', value: 'Partially Received' },
    { label: 'Product Received', value: 'Product Received' },
    { label: 'Invoice', value: 'Invoice' },
    { label: 'Completed', value: 'Completed' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('Processing');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const getPoData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    bill_status: activeButton,
    global_search: '',
    project_id: selectedValue,
    purchase_order_type: 'Head Office',
  };
  /* Function to get all purchase order data */
  const {
    isLoading: dataLoading,
    data: getAllData,
    refetch,
  } = useGetAllPurchaseOrderData(getPoData);
  const orderType = {
    purchase_order_type: 'Head Office',
  };
  const { isLoading: PODataLoading, data: POData } =
    usePurchaseOrderGetAll(orderType);
  const { data: getAllProjectDataForDrop = [], isLoading: dropLoading } =
    useGetAllProject();
  const handleEdit = (value: any) => {
    setPurchaseId(value);
    setShowEditPopUp(true);
  };
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  /* Function to change page */
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
  /* Function for generating pdf report */
  const handleReportGenerator = async (data: any) => {
    await PurchaseOrderReport(data);
  };
  /* Funtion for generating excel report */
  const handleExcelReportGenerator = async (data: any) => {
    await PurchaseOrderExcelReport(data);
  };
  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, selectedValue, activeButton]);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  /* Function to give custom name to the file in the table list */
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
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
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
            </div>
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              color="primary"
              icon={<PrintIcon color="white" />}
              onClick={() => handleExcelReportGenerator(POData?.data)}
            >
              PO Report
            </Button>
          </div>
          <div className={Styles.groupButton}>
            <CustomGroupButton
              labels={buttonLabels}
              onClick={handleGroupButtonClick}
              activeButton={activeButton}
            />
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
                  <th className={Styles.tableHeading}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {getAllData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    <td></td>
                    <td></td>
                    <td></td>
                  </tr>
                ) : (
                  getAllData?.content?.map((data: any, index: number) => {
                    const customBillName = generateCustomBillName(data);
                    const customQuotationName =
                      generateCustomQuotationName(data);
                    return (
                      <tr key={data?.purchase_order_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data?.vendor_data?.vendor_name}</td>
                        <td>
                          {
                            data?.purchase_request_data?.project_data
                              ?.project_name
                          }
                        </td>
                        <td>
                          {formatBudgetValue(
                            data?.total_cost ? data?.total_cost : 0
                          )}
                        </td>
                        <td>
                          <div>
                            {data?.purchase_request_data
                              ?.purchase_request_documents?.length > 0 ? (
                              data?.purchase_request_data?.purchase_request_documents.map(
                                (document: any, index: number) => (
                                  <div
                                    key={
                                      data?.purchase_request_data
                                        ?.purchase_request_id
                                    }
                                  >
                                    <a
                                      href={document.path}
                                      target="_blank"
                                      rel="noopener noreferrer"
                                    >
                                      {customQuotationName}
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
                          <div className={Styles.tablerow}>
                            <ViewIcon
                              onClick={() =>
                                navigate(
                                  `/purchase-order-view/${data.purchase_order_id}`
                                )
                              }
                            />
                            <PdfDownloadIcon
                              onClick={() => handleReportGenerator(data)}
                            />
                          </div>
                        </td>
                      </tr>
                    );
                  })
                )}
              </tbody>
            </table>
          </div>
        </div>
        <div className={Styles.pagination}>
          <CustomPagination
            currentPage={currentPage}
            totalPages={getAllData?.total_page}
            totalCount={getAllData?.total_count}
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
