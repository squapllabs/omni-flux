import React, { useEffect, useState } from 'react';
import Styles from '../../../../styles/project.module.scss';
import {
  getProjectBasedIndent,
  getBySearchIndent,
} from '../../../../hooks/indentRequest-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import EditIcon from '../../../menu/icons/editIcon';
import { format } from 'date-fns';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import { formatBudgetValue } from '../../../../helper/common-function';
import Select from '../../../ui/selectNew';
import Pagination from '../../../menu/pagination';
import CustomLoader from '../../../ui/customLoader';
const ProjectIndentRequestList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const approverStatus: any = [
    { value: 'Approved', label: 'Approved' },
    { value: 'Pending', label: 'Pending' },
    { value: 'Rejected', label: 'Rejected' },
  ];
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(5);
  const [filterValues, setFilterValues] = useState({
    approver_status: '',
  });

  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getBySearchIndent();
  // console.log('getFilterData', getFilterData);

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);

  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      project_id: Number(routeParams?.id),
      ...filterValues,
    };
    // console.log('demo', demo);
    postDataForFilter(demo);
    // setIsLoading(false);
    // setFilter(true);
  };

  /* Function for resting the search field and data to normal state */
  const handleReset = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(demo);
    setFilterValues({
      approver_status: '',
    });
  };
  /* Function for changing the table page */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  /* Function for changing no of rows in pagination */
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['approver_status']: event.target.value,
    });
  };

  const { data: getIndentList } = getProjectBasedIndent(
    Number(routeParams?.id)
  );
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'dd-MM-yyyy');
    return formattedDate;
  };

  return (
    <div>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.box}>
          <div className={Styles.headingContent}>
            <div className={Styles.textContent_1}>
              <h3>Indent Request</h3>
              <span className={Styles.content}>Add Indent Request</span>
            </div>
            <div>
              <Button
                type="button"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
                icon={<AddIcon width={20} color="white" />}
                onClick={(e) => {
                  navigate(`/indent/${routeParams?.id}`);
                }}
              >
                Add
              </Button>
            </div>
          </div>
        </div>
        <div className={Styles.searchField}>
          <div className={Styles.inputFilter}>
            <div className={Styles.filterSelect}>
              <Select
                label="Indent Status"
                name="approver_status"
                value={filterValues.approver_status}
                onChange={(e) => handleFilterChange(e)}
                defaultLabel="Select from options"
                placeholder="Select from options"
              >
                {approverStatus?.map((items: any, index: any) => {
                  return (
                    <option key={items.value} value={items.value}>
                      {items.label}
                    </option>
                  );
                })}
              </Select>
            </div>
            <div className={Styles.filterButton}>
              <Button
                className={Styles.searchButton}
                type="button"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
                onClick={(e) => handleSearch(e)}
              >
                Search
              </Button>
              <Button
                className={Styles.resetButton}
                type="button"
                color="secondary"
                shape="rectangle"
                size="small"
                justify="center"
                onClick={(e) => handleReset(e)}
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
                  <th className={Styles.tableHeading}>S No</th>
                  <th className={Styles.tableHeadingSite}>Requested Date</th>
                  <th className={Styles.tableHeading}>
                    Expected Delivery Date
                  </th>
                  <th className={Styles.tableHeading}>Cost</th>
                  <th className={Styles.tableHeading}>Status</th>
                  <th className={Styles.tableHeading}>Action</th>
                </tr>
              </thead>
              <tbody>
                {getFilterData?.content?.length > 0 ? (
                  getFilterData.content.map((items: any, index: any) => {
                    rowIndex = rowIndex + 1;
                    return (
                      <tr key={index}>
                        <td>{rowIndex}</td>
                        <td>{dateFormat(items?.requested_date)}</td>
                        <td>{dateFormat(items?.expected_delivery_date)}</td>
                        <td>{formatBudgetValue(items?.total_cost)}</td>
                        <td>{items?.approver_status}</td>
                        <td>
                          <div
                            style={{
                              cursor: 'pointer',
                            }}
                          >
                            <EditIcon
                              onClick={(e) => {
                                navigate(
                                  `/indent/${routeParams?.id}/${items?.indent_request_id}`
                                );
                              }}
                            />
                          </div>
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
          <div className={Styles.pagination}>
            <Pagination
              currentPage={currentPage}
              totalPages={getFilterData?.total_page}
              totalCount={getFilterData?.total_count}
              rowsPerPage={rowsPerPage}
              onPageChange={handlePageChange}
              onRowsPerPageChange={handleRowsPerPageChange}
            />
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};
export default ProjectIndentRequestList;
