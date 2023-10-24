import React, { useEffect, useState } from 'react';
import Styles from '../../../../styles/newStyles/projectIndentList.module.scss';
import {
  getProjectBasedIndent,
  getBySearchIndent,
  getIndentSearchPaginated,
} from '../../../../hooks/indentRequest-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import EditIcon from '../../../menu/icons/newEditIcon';
import { format } from 'date-fns';
import Button from '../../../ui/Button';
import AddIcon from '../../../menu/icons/addIcon';
import { formatBudgetValue } from '../../../../helper/common-function';
import Select from '../../../ui/selectNew';
import CustomLoader from '../../../ui/customLoader';
import CustomPagination from '../../../menu/CustomPagination';
import BOQIcon from '../../../menu/icons/boqIcon';
import Input from '../../../ui/Input';
import SearchIcon from '../../../menu/icons/search';
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
    search_by_code: '',
  });

  const demo: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    project_id: Number(routeParams?.id),
    indent_request_code: filterValues.search_by_code,
    approver_status: filterValues.approver_status,
  };
  const {
    isLoading: FilterLoading,
    data: getFilterData,
    refetch,
  } = getIndentSearchPaginated(demo);
  console.log('getFilterData', getFilterData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, filterValues]);
  useEffect(() => {
    if (filterValues?.search_by_code != '') {
      const handleSearch = setTimeout(() => {
        refetch();
      }, 1000);
      return () => clearTimeout(handleSearch);
    }
  }, [filterValues]);

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
    // const searchValue = event.target.value;
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
        {getFilterData?.total_count !== 0 ||
        filterValues.approver_status !== '' ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.headingOne}>
                  <div className={Styles.subHeading}>
                    <BOQIcon />
                    <h3>Indent Request</h3>
                  </div>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon color="white" />}
                      onClick={(e) => {
                        navigate(`/indent/${routeParams?.id}`);
                      }}
                    >
                      Add
                    </Button>
                  </div>
                </div>
                <div className={Styles.searchBar} style={{ gap: '10px' }}>
                  <Input
                    width="260px"
                    prefixIcon={<SearchIcon />}
                    name="search_by_code"
                    value={filterValues.search_by_code}
                    onChange={(e) => {
                      setFilterValues({
                        ...filterValues,
                        ['search_by_code']: e.target.value,
                      });
                      setCurrentPage(1);
                    }}
                    placeholder="Search by Code"
                  />
                  <Select
                    width="200px"
                    name="approver_status"
                    value={filterValues.approver_status}
                    onChange={(e) => handleFilterChange(e)}
                    defaultLabel="Select from options"
                    placeholder="Select All"
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
              </div>
            </div>
            {/* <div className={Styles.searchField}>
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
                </div> */}
            {/* <div className={Styles.filterButton}>
                  <Button
                    // className={Styles.searchButton}
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
                </div> */}
            {/* </div>
            </div> */}
            <div className={Styles.tableContainer}>
              <div>
                <table className={Styles.scrollable_table}>
                  <thead>
                    <tr>
                      <th className={Styles.tableHeading}>#</th>
                      <th className={Styles.tableHeading}>Indent Code</th>
                      <th className={Styles.tableHeadingSite}>
                        Indent Requested Date
                      </th>
                      <th className={Styles.tableHeading}>
                        Expected Delivery Date
                      </th>
                      <th className={Styles.tableHeading}>Indent Status</th>
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
                            <td>{items?.indent_request_code}</td>
                            <td>{dateFormat(items?.requested_date)}</td>
                            <td>{dateFormat(items?.expected_delivery_date)}</td>
                            {/* <td>{formatBudgetValue(items?.total_cost)}</td> */}
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
                <CustomPagination
                  currentPage={currentPage}
                  totalPages={getFilterData?.total_page}
                  totalCount={getFilterData?.total_count}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        ) : (
          <div>
            <div className={Styles.subHeading}>
              <BOQIcon width={30} height={30} color="black" />
              <h3>Indent Raise</h3>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.image}>
                <img src="/boq-add.png" width="100%" height="20%" />
              </div>
              <div>
                <h5 className={Styles.textmax}>
                  No indent added to this Project
                </h5>
              </div>
              <div>
                <p className={Styles.textmin}>
                  Go ahead, add a indent to this project now
                </p>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={(e) => {
                    navigate(`/indent/${routeParams?.id}`);
                  }}
                >
                  Add
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
    </div>
  );
};
export default ProjectIndentRequestList;
