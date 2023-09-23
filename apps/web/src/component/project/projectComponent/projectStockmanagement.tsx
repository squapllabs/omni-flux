import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/project.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import { useNavigate, useParams } from 'react-router-dom';
import {
  useGetAllStockAudits,
  getByFilterStockAudit,
} from '../../../hooks/stockAudit-hooks';
import CustomLoader from '../../ui/customLoader';
import { format } from 'date-fns';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import { getProjectSite } from '../../../hooks/project-hooks';
import Pagination from '../../menu/pagination';
import ViewIcon from '../../menu/icons/viewIcon';

const ProjectStockmanagement = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  let rowIndex = 0;
  const {
    mutate: postDataForFilter,
    data: getStockAuditList,
    isLoading: fetchLoader,
  } = getByFilterStockAudit();
  console.log('getStockAuditList', getStockAuditList);

  const { data: getSiteList } = getProjectSite(Number(routeParams?.id));
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
      <CustomLoader loading={fetchLoader}>
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
            <div className={Styles.buttons}>
              <Button
                type="button"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
                icon={<AddIcon width={20} color="white" />}
                onClick={() => {
                  navigate(`/project-stockadd/${routeParams?.id}`);
                }}
              >
                Add
              </Button>
            </div>
          </div>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S.No</th>
                <th>Site</th>
                <th>Audit Date</th>
                <th>Item count</th>
                <th>Action</th>
              </tr>
            </thead>
            <tbody>
              {getStockAuditList?.content?.map((items: any, index: any) => {
                rowIndex = rowIndex + 1;
                return (
                  <tr>
                    <td>{rowIndex}</td>
                    <td>{items?.site_data?.name}</td>
                    <td>{dateFormat(items?.stock_audit_date)}</td>
                    <td>{items?.item_details?.length}</td>
                    <td>
                      <div
                        style={{ cursor: 'pointer' }}
                        onClick={() => {
                          navigate(
                            `/project-stockView/${items?.stock_audit_id}`
                          );
                        }}
                      >
                        <ViewIcon />
                      </div>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
          <div className={Styles.pagination}>
            <Pagination
              currentPage={currentPage}
              totalPages={getStockAuditList?.total_page}
              totalCount={getStockAuditList?.total_count}
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

export default ProjectStockmanagement;
