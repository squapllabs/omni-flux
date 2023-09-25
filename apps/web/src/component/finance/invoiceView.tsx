import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {
  useGetAllPurchaseOrderData,
  getBySearchPoData,
} from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import CustomLoader from '../ui/customLoader';
import EditIcon from '../menu/icons/editIcon';
import CustomEditPoPopup from '../ui/CustomEditPoPopup';
import { formatBudgetValue } from '../../helper/common-function';
import Pagination from '../menu/pagination';
import Button from '../ui/Button';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllProject } from '../../hooks/project-hooks';

const OrderView = () => {
  const navigate = useNavigate();
  const [showEditPopUp, setShowEditPopUp] = useState(false);
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
    bill_status:'Invoice',
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
  // console.log('getFilterData---->', getFilterData);

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

  const handleSearch = async () => {
    const poData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'AC',
      global_search: '',
      bill_status:'Invoice',
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

  return (
    <div className={Styles.container}>
      <CustomLoader
        loading={searchLoader ? searchLoader : dataLoading}
        size={48}
        color="#333C44"
      >
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Purchase Order Invoice Details</h3>
            <span className={Styles.content}>
              Manage invoice here your entire organization.
            </span>
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
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Vendor Name</th>
                  <th>Project Name </th>
                  <th>Budget</th>
                  <th>Quotation </th>
                  <th>Bill Status</th>
                  <th>Bill</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {dataShow ? 
                 getFilterData?.content?.map((data: any, index: number) => {
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
                                    Uploaded Document
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
                                    Uploaded Document
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
                            onClick={() => handleEdit(data.purchase_order_id)}
                          />
                        </div>
                      </td>
                    </tr>
                  );
                }) : 
                getAllData?.content?.map((data: any, index: number) => {
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
                                    Uploaded Document
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
                                    Uploaded Document
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
                            onClick={() => handleEdit(data.purchase_order_id)}
                          />
                        </div>
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
          <CustomEditPoPopup
            isVissible={showEditPopUp}
            onAction={setShowEditPopUp}
            selectedPurchaseOrder={purchaseId}
          />
        </div>
        <div className={Styles.pagination}>
          <Pagination
            currentPage={currentPage}
            totalPages={ dataShow ? getFilterData?.total_page : getAllData?.total_page}
            totalCount={ dataShow ? getFilterData?.total_count : getAllData?.total_count}
            rowsPerPage={rowsPerPage}
            onPageChange={handlePageChange}
            onRowsPerPageChange={handleRowsPerPageChange}
          />
        </div>
      </CustomLoader>
    </div>
  );
};

export default OrderView;