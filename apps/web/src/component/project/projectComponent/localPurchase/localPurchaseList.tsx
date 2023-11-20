import React, { useState, useEffect } from 'react';
import Styles from '../../../../styles/newStyles/localPurchase.module.scss';
import Select from '../../../ui/selectNew';
import PurchaseIcon from '../../../menu/icons/purchaseIcon';
import Input from '../../../ui/Input';
import SearchIcon from '../../../menu/icons/search';
import { useNavigate, useParams } from 'react-router-dom';
import { useGetIndentSearchPaginated } from '../../../../hooks/indentRequest-hooks';
import { format } from 'date-fns';
import { formatBudgetValue } from '../../../../helper/common-function';
import CustomPagination from '../../../menu/CustomPagination';
import CustomLoader from '../../../ui/customLoader';
import ViewIcon from '../../../menu/icons/newViewIcon';

const LocalPurchaseList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const projectId = Number(routeParams?.id);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [filterValues, setFilterValues] = useState({
    priority: '',
    search_by_code: '',
  });

  const indentData: any = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'created_date',
    order_by_direction: 'desc',
    project_id: projectId,
    priority: filterValues.priority,
    status: 'AC',
    // approver_status: 'Approved',
    request_type: 'Local Purchase',
    global_search: filterValues.search_by_code,
  };

  const {
    isLoading: dataLoading,
    data: initialData,
    refetch,
  } = useGetIndentSearchPaginated(indentData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const options: any = [
    { value: 'Low', label: 'Low' },
    { value: 'Medium', label: 'Medium' },
    { value: 'High', label: 'High' },
  ];

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div>
      <CustomLoader loading={dataLoading} size={48} color="#333C44">
        {initialData?.is_available ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.headingOne}>
                  <div className={Styles.subHeading}>
                    <PurchaseIcon />
                    <h3>Local Purchase</h3>
                  </div>
                </div>
                <div className={Styles.searchBar} style={{ gap: '10px' }}>
                  <Select
                    width="180px"
                    name="priority"
                    defaultLabel="Select from options"
                    placeholder="Priority"
                    value={filterValues?.priority}
                    onChange={(e) => {
                      setFilterValues({
                        ...filterValues,
                        priority: e.target.value,
                      });
                      setCurrentPage(1);
                    }}
                  >
                    {options?.map((item: any, index: any) => {
                      return <option value={item.value}>{item.label}</option>;
                    })}
                  </Select>
                  <Input
                    width="200px"
                    prefixIcon={<SearchIcon />}
                    name="search_by_code"
                    value={filterValues.search_by_code}
                    onChange={(e) => {
                      setFilterValues({
                        ...filterValues,
                        search_by_code: e.target.value,
                      });
                      setCurrentPage(1);
                    }}
                    placeholder="Search by Indent Code"
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
                      <th className={Styles.tableHeading}>Indent Code</th>
                      {/* <th className={Styles.tableHeading}>Project Name</th> */}
                      <th className={Styles.tableHeading}>Raised By</th>
                      <th className={Styles.tableHeading}>
                        Expected Delivery Date
                      </th>
                      <th className={Styles.tableHeading}>Priority</th>
                      <th className={Styles.tableHeading}>Cost</th>
                      <th className={Styles.tableHeading}>Approved By</th>
                      <th className={Styles.tableHeading}>Approved Date</th>
                      <th className={Styles.tableHeading}>Actions</th>
                    </tr>
                  </thead>
                  <tbody>
                    {initialData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        <td></td>
                      </tr>
                    ) : (
                      ''
                    )}
                    {initialData?.content?.map((data: any, index: number) => {
                      return (
                        <tr key={data?.indent_request_id}>
                          <td>{startingIndex + index}</td>
                          <td>{data?.indent_request_code}</td>
                          {/* <td>{data?.project_data?.project_name}</td> */}
                          <td>
                            {data?.requester_user_data?.first_name +
                              ' ' +
                              data?.requester_user_data?.last_name}
                          </td>
                          <td>
                            {format(
                              new Date(data?.expected_delivery_date),
                              'MMM dd, yyyy'
                            )}
                          </td>
                          <td
                            className={
                              data?.priority === 'HIGH'
                                ? Styles.highPriority
                                : ''
                            }
                          >
                            {data?.priority}
                          </td>
                          <td>
                            {formatBudgetValue(
                              data?.total_cost ? data?.total_cost : 0
                            )}
                          </td>
                          <td>
                            {data?.approver_user_data?.first_name +
                              ' ' +
                              data?.approver_user_data?.last_name}
                          </td>
                          <td>
                            {format(
                              new Date(data?.approved_date),
                              'MMM dd, yyyy'
                            )}
                          </td>
                          <td>
                            <ViewIcon
                              onClick={() =>
                                navigate(
                                  `/local-purchase-order/${data?.indent_request_id}`,
                                  {
                                    state: { project_id: projectId },
                                  }
                                )
                              }
                            />
                          </td>
                        </tr>
                      );
                    })}
                  </tbody>
                </table>
              </div>
              <div className={Styles.pagination}>
                <CustomPagination
                  currentPage={currentPage}
                  totalPages={initialData?.total_page}
                  totalCount={initialData?.total_count}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        ) : (
          <div>
            <div className={Styles.headingOne}>
              <div className={Styles.subHeading}>
                <PurchaseIcon />
                <h3>Local Purchase</h3>
              </div>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/localPurchase.jpg"
                  alt="masterData_img"
                  width="100%"
                  height="250px"
                />
              </div>
              <div>
                <h5>No Local purchase request have been Raised</h5>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
    </div>
  );
};
export default LocalPurchaseList;
