import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectWorkBreakDownList.module.scss';
import SearchIcon from '../menu/icons/search';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Pagination from '../menu/pagination';
import CustomLoader from '../ui/customLoader';
import {getBySearchSiteData
} from '../../hooks/site-hooks';
import AddIcon from '../menu/icons/addIcon';
import { useNavigate } from 'react-router';
import EditIcon from '../menu/icons/editIcon';

const ProjectWorkBreakList = () => {
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [disable, setDisable] = useState(true);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(3); // Set initial value to 1
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getBySearchSiteData();
  const navigate = useNavigate();

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };

  

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
      type:"Site"
    };
    await postDataForFilter(demo);
    setTotalPages(getFilterData?.total_page);
    setIsLoading(false);
    setFilter(true);
    setDisable(false);
  };
  const handleReset = async () => {
    setFilterValues({
      search_by_name: '',
    });
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setIsLoading(false);
    setDisable(true);
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
  return (
    <div>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.container}>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Project Site</h3>
              <span className={Styles.content}>
                Manage your project site across your application
              </span>
            </div>
            <div className={Styles.searchField}>
              <div className={Styles.inputFilter}>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="search_by_name"
                  value={filterValues.search_by_name}
                  onChange={(e) => handleFilterChange(e)}
                  placeholder="Search by name"
                />
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
                  disabled={disable}
                  onClick={handleReset}
                >
                  Reset
                </Button>
              </div>
              <div>
                <Button
                  className={Styles.resetButton}
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon />}
                  onClick={() => navigate('/site-add')}
                >
                  Add
                </Button>
              </div>
            </div>
            <div className={Styles.tableContainer}>
              <div>
                <table>
                  <thead>
                    <tr>
                      <th>S No</th>
                      <th>Name</th>
                      <th>Mobile Number</th>
                      <th>Description</th>
                      <th></th>
                    </tr>
                  </thead>
                  <tbody>
                    {getFilterData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        <td></td>
                      </tr>
                    ) : (
                      ''
                    )}
                    {getFilterData?.content?.map((item: any, index: number) => (
                      <tr key={item.site_contractor_id}>
                        <td>{index + 1}</td>
                        <td>{item.name}</td>
                        <td>{item.mobile_number}</td>
                        <td>{item.description}</td>
                        <td>
                          <EditIcon
                            onClick={() =>
                              navigate(
                                `/site-edit/${item.site_contractor_id}`
                              )
                            }
                          />
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
              <div className={Styles.pagination}>
                <Pagination
                  currentPage={currentPage}
                  totalPages={getFilterData?.total_page}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default ProjectWorkBreakList;