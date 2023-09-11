import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectlist.module.scss';
import {
  getByProject,
  useDeleteProjects,
  useGetAllProject,
} from '../../hooks/project-hooks';
import Input from '../ui/Input';
import Button from '../ui/Button';
import SearchIcon from '../menu/icons/search';
import AddIcon from '../menu/icons/addIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import { format } from 'date-fns';
import Pagination from '../menu/pagination';
import EditIcon from '../menu/icons/editIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import { useNavigate } from 'react-router-dom';
import CustomLoader from '../ui/customLoader';
import ViewIcon from '../menu/icons/viewIcon';

const ProjectList = () => {
  const { isLoading: getAllLoading } = useGetAllProject();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getByProject();
  const { mutate: getDeleteProjectByID } = useDeleteProjects();

  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [filter, setFilter] = useState(false);
  const [isLoading, setIsLoading] = useState(true);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');
  const [isResetDisabled, setIsResetDisabled] = useState(true);
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
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      status: activeButton,
    };
    postDataForFilter(userData);
    setIsLoading(false);
    setFilter(true);
  };

  /* Function for reseting the table to its actual state after search */
  const handleReset = async () => {
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_by',
      order_by_direction: 'desc',
      global_search: '',
      status: 'AC',
    };
    postDataForFilter(userData);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
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

  const deleteProjectHandler = (id: any) => {
    setValue(id);
    setOpen(true);
  };

  const deleteProject = () => {
    getDeleteProjectByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenDeleteSnack(true);
  };

  const handleClose = () => {
    setOpen(false);
  };

  const handleSnackBarClose = () => {
    setOpenDeleteSnack(false);
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <div>
        <CustomLoader
          loading={isLoading === true ? getAllLoading : FilterLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.text}>
            <div className={Styles.textStyle}>
              <h3>List of Projects</h3>
            </div>
            <div className={Styles.textStyle}>
              <h6>Project List</h6>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <Input
                width="260px"
                prefixIcon={<SearchIcon />}
                name="search_by_name"
                value={filterValues.search_by_name}
                onChange={(e) => handleFilterChange(e)}
                placeholder="Search"
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
                onClick={handleReset}
                disabled={isResetDisabled}
              >
                Reset
              </Button>
            </div>
            <div className={Styles.button}>
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
              <div>
                <Button
                  shape="rectangle"
                  justify="center"
                  size="small"
                  color="primary"
                  icon={<AddIcon />}
                  onClick={() => navigate('/project')}
                >
                  Add
                </Button>
              </div>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.tableContainer}>
            <div>
              <table>
                <thead>
                  <tr>
                    <th>S. No</th>
                    <th>Name</th>
                    <th>Code</th>
                    <th>Manager</th>
                    <th>Status</th>
                    <th>Start Date</th>
                    <th>End Date</th>
                    {activeButton === 'AC' && <th></th>}
                  </tr>
                </thead>
                <tbody>
                  {getFilterData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td></td>
                      <td>No data found</td>
                      {activeButton === 'AC' && <td></td>}
                    </tr>
                  ) : (
                    ''
                  )}
                  {getFilterData?.content?.map((data: any, index: number) => (
                    <tr key={data.user_id}>
                      <td>{startingIndex + index}</td>
                      <td>{data.project_name}</td>
                      <td>{data.code}</td>
                      <td>
                        {data.user?.first_name} {data.user?.last_name}
                      </td>
                      <td>{data.status}</td>
                      <td>
                        {format(new Date(data.date_started), 'MMM dd, yyyy')}
                      </td>
                      <td>
                        {format(new Date(data.date_ended), 'MMM dd, yyyy')}
                      </td>
                      {activeButton === 'AC' && (
                        <td>
                          <div className={Styles.tablerow}>
                            <EditIcon
                              onClick={() =>
                                navigate(`/project-edit/${data.project_id}`)
                              }
                            />
                            <ViewIcon
                              onClick={() =>
                                navigate(`/project-info/${data.project_id}`)
                              }
                            />
                            {/* <DeleteIcon
                            onClick={() =>
                              deleteProjectHandler(data.project_id)
                            }
                          /> */}
                          </div>
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
        </CustomLoader>
        <CustomDelete
          open={open}
          handleClose={handleClose}
          title="Delete Project "
          contentLine1="Are you want to delete this Project?"
          contentLine2=""
          handleConfirm={deleteProject}
        />
        <CustomSnackBar
          open={openDeleteSnack}
          message={message}
          onClose={handleSnackBarClose}
          type="success"
          autoHideDuration={1000}
        />
      </div>
    </div>
  );
};

export default ProjectList;
