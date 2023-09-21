import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import {useGetAllPurchaseOrderData } from '../../hooks/purchase-request-hooks';
import Styles from '../../styles/purchaseRequestView.module.scss';
import CustomLoader from '../ui/customLoader';
import EditIcon from '../menu/icons/editIcon';
import CustomEditPoPopup from '../ui/CustomEditPoPopup';
import { formatBudgetValue } from '../../helper/common-function';
import Pagination from '../menu/pagination';

const OrderView = () => {
  const navigate = useNavigate();
  const [showEditPopUp, setShowEditPopUp] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [purchaseId,setPurchaseId] = useState();

  const getPoData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    global_search:'',
  };
  const {
    isLoading: dataLoading,
    data: getAllData,
    refetch,
  } = useGetAllPurchaseOrderData(getPoData);

  const handleEdit = (value: any) => {
    setPurchaseId(value)
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


  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage]);
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        <div className={Styles.headingTop}>
          <div className={Styles.textContent}>
            <h5>
              Manage purchase order your entire organisation
            </h5>
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
                {getAllData?.content?.map(
                  (data: any, index: number) => {
                    return (
                      <tr>
                        <td>{startingIndex + index}</td>
                        <td>{data?.vendor_data?.vendor_name}</td>
                        <td>{data?.purchase_request_data?.project_data?.project_name}</td>
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
                            <EditIcon onClick={() => handleEdit(data.purchase_order_id)} />
                          </div>
                        </td>
                      </tr>
                    );
                  }
                )}
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
            totalPages={getAllData?.total_page}
            totalCount={getAllData?.total_count}
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
