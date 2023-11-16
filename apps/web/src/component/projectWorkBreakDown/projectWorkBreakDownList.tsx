import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectWorkBreakDownList.module.scss';
import SearchIcon from '../menu/icons/search';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Select from '../ui/selectNew';
import Pagination from '../menu/pagination';
import CustomLoader from '../ui/customLoader';
import {
  useGetAllParentProjectBreakDownDrop,
  useGetBySearchProjectWorkBreakDownData,
} from '../../hooks/projectBreakDown-hook';
import { formatBudgetValue } from '../../helper/common-function';
import AddIcon from '../menu/icons/addIcon';
import { useNavigate } from 'react-router';
import EditIcon from '../menu/icons/editIcon';

const ProjectWorkBreakList = () => {
  const { data: getAllParentDatadrop = [] } =
    useGetAllParentProjectBreakDownDrop();
  const [selectedValue, setSelectedValue] = useState('');
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
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
  } = useGetBySearchProjectWorkBreakDownData();
  const navigate = useNavigate();

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      handleReset();
    }
  };

  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
    setIsResetDisabled(searchValue === '');
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
      parent_id: Number(selectedValue),
    };
    await postDataForFilter(demo);
    setTotalPages(getFilterData?.total_page);
    setIsLoading(false);
    setFilter(true);
  };
  const handleReset = async () => {
    setFilterValues({
      search_by_name: '',
    });
    setSelectedValue('');
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
    setIsResetDisabled(true);
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

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div className={Styles.container}>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Project Work Break Down</h3>
              <span className={Styles.content}>
                Manage your project work break down across your application
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
                <div className={Styles.input}>
                  <Select
                    name="parent_project_workbreak_down_id"
                    onChange={handleDropdownChange}
                    value={selectedValue}
                    defaultLabel="Select from options"
                    width="200px"
                  >
                    {getAllParentDatadrop.map((option: any) => (
                      <option key={option.value} value={option.value}>
                        {option.label}
                      </option>
                    ))}
                  </Select>
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
              <div>
                <Button
                  className={Styles.resetButton}
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon />}
                  onClick={() => navigate('/project-workbreakdown-add')}
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
                      <th>Description</th>
                      <th>Code</th>
                      <th>Rate</th>
                      <th>Parent Name</th>
                      {activeButton === 'AC' && <th></th>}
                    </tr>
                  </thead>
                  <tbody>
                    {getFilterData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        {activeButton === 'AC' && <td></td>}
                      </tr>
                    ) : (
                      ''
                    )}
                    {getFilterData?.content?.map((item: any, index: number) => (
                      <tr key={item.project_workbreak_down_id}>
                        <td>{startingIndex + index}</td>
                        <td>{item.project_workbreak_down_name}</td>
                        <td>
                          <span
                            className={Styles.truncatedStyle}
                            title={item.project_workbreak_down_description}
                          >
                            {item.project_workbreak_down_description}
                          </span>
                        </td>
                        <td>{item.project_workbreak_down_code}</td>
                        <td>{formatBudgetValue(item.rate)}</td>
                        <td>
                          {item?.parent_project_workbreak_down
                            ?.project_workbreak_down_name === undefined
                            ? '-'
                            : item?.parent_project_workbreak_down
                                ?.project_workbreak_down_name}
                        </td>
                        {activeButton === 'AC' && (
                          <td>
                            <EditIcon
                              onClick={() =>
                                navigate(
                                  `/project-workbreakdown-edit/${item.project_workbreak_down_id}`
                                )
                              }
                            />
                          </td>
                        )}
                      </tr>
                    ))}
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
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default ProjectWorkBreakList;
