import React, { useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import Styles from '../../styles/project.module.scss';
// import Pagination from '../menu/pagination';
import { useGetBySearchsiteExpense } from '../../hooks/expense-hook';
import { useGetProjectSite } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';
import EditIcon from '../menu/icons/newEditIcon';
import CustomPagination from '../menu/CustomPagination';

const SiteExpenseList = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const {
    mutate: postDataForFilter,
    data: getExpenseList,
    isLoading: fetchLoader,
  } = useGetBySearchsiteExpense();
  // console.log('getExpenseList', getExpenseList?.content);

  const { data: getSiteList } = useGetProjectSite(Number(routeParams?.id));
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [filterValue, setFilterValue] = useState<any>({});

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);

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

  /* Function for search */
  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      project_id: Number(routeParams?.id),
      ...filterValue,
    };
    postDataForFilter(demo);
  };

  /* Function for resting the search field and data to normal state */
  const handleReset = async () => {
    setFilterValue('');
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
      project_id: Number(routeParams?.id),
      site_id: '',
    };
    postDataForFilter(demo);
  };
  return (
    <div>
      <CustomLoader loading={fetchLoader} size={48}>
        <div className={Styles.box}>
          <div className={Styles.headingContent}>
            <div className={Styles.textContent_1}>
              <h3>Site Expenses</h3>
              <span className={Styles.content}>Manage your site expenses</span>
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
                  navigate(`/expenses/${routeParams?.id}`);
                }}
              >
                Add
              </Button>
            </div>
          </div>
        </div>

        <div className={Styles.tableContainer}>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div className={Styles.filterSelect}>
                <AutoCompleteSelect
                  name="site_id"
                  label="Site"
                  mandatory={true}
                  optionList={getSiteList}
                  value={filterValue.site_id}
                  onSelect={(value) => {
                    setFilterValue({ ...filterValue, ['site_id']: value });
                  }}
                />
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
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>#</th>
                <th className={Styles.tableHeading}>Site</th>
                <th className={Styles.tableHeading}>From Date</th>
                <th className={Styles.tableHeading}>To Date</th>
                <th className={Styles.tableHeading}>Amount</th>
                <th className={Styles.tableHeading}>Status</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            <tbody>
              {getExpenseList?.content?.length === 0 ? (
                <tr>
                  <td colSpan="7" style={{ textAlign: 'center' }}>
                    No data found
                  </td>
                </tr>
              ) : (
                ''
              )}
              {getExpenseList?.content?.map((items: any, index: any) => {
                if (items.is_delete !== true) {
                  rowIndex = rowIndex + 1;
                  console.log('items', items);
                  const sumOfRates = items?.expense_details.reduce(
                    (accumulator: any, currentItem: any) => {
                      return accumulator + currentItem.total;
                    },
                    0
                  );
                  return (
                    <tr>
                      <td>{rowIndex}</td>
                      <td>{items?.site_data?.name}</td>
                      <td>{dateFormat(items?.start_date)}</td>
                      <td>{dateFormat(items?.end_date)}</td>
                      <td>{sumOfRates}</td>
                      <td>{items?.status}</td>
                      <td>
                        <div
                          style={{ cursor: 'pointer' }}
                          onClick={() => {
                            navigate(
                              `/expenses-edit/${routeParams?.id}/${items.expense_id}`
                            );
                          }}
                        >
                          <EditIcon />
                        </div>
                      </td>
                    </tr>
                  );
                }
              })}
            </tbody>
          </table>
          <div className={Styles.pagination}>
            <CustomPagination
              currentPage={currentPage}
              totalPages={getExpenseList?.total_page}
              totalCount={getExpenseList?.total_count}
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

export default SiteExpenseList;
