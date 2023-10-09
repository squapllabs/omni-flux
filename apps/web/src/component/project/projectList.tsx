import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectlist.module.scss';
import {
  getByProject,
  useDeleteProjects,
  getMemberBasedProject,
  useGetAllProject,
} from '../../hooks/project-hooks';
import Input from '../ui/Input';
import Button from '../ui/Button';
import SearchIcon from '../menu/icons/search';
import AddIcon from '../menu/icons/addIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import { format } from 'date-fns';
import Pagination from '../menu/pagination';
import EditIcon from '../menu/icons/newEditIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import { useNavigate } from 'react-router-dom';
import CustomLoader from '../ui/customLoader';
import ViewIcon from '../menu/icons/newViewIcon';
import CustomCard from '../ui/CustomCard';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import StoreIcon from '../menu/icons/newStoreIcon';
import { Chart } from 'react-google-charts';
import CustomPagination from '../menu/CustomPagination';
import ProjectSubheader from './projectSubheader';

const ProjectList = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const isProjectCreate =
    roleName === 'PROJECT MANAGER' || roleName === 'ADMIN';
  const isProjectEdit =
    roleName === 'PROJECT MANAGER' ||
    roleName === 'ADMIN' ||
    roleName === 'SITE MANAGER' ||
    roleName === 'PLANNING ENGINEER';
  const { isLoading: getAllLoading } = useGetAllProject();

  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getMemberBasedProject();

  const { mutate: getDeleteProjectByID } = useDeleteProjects();
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [buttonLabels, setButtonLabels] = useState([
    ...(roleName === 'PROJECT MANAGER' || roleName === 'ADMIN'
      ? [
          { label: 'All', value: 'ALL' },
          { label: 'Draft', value: 'Draft' },
        ]
      : []),
    { label: 'Inprogress', value: 'Inprogress' },
    { label: 'Completed', value: 'Completed' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('Inprogress');
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
    setCurrentPage(1);
  };
  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  /* Function for searching a user in the table */
  const handleSearch = async () => {
    const allData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      status: activeButton,
    };
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      status: 'AC',
      user_id: roleName === 'ADMIN' ? null : userID,
      project_status: activeButton,
      project_manager_id: roleName === 'PROJECT MANAGER' ? true : false,
    };
    postDataForFilter(activeButton === 'ALL' ? allData : userData);
    setIsLoading(false);
    setFilter(true);
  };

  /* Function for reseting the table to its actual state after search */
  const handleReset = async () => {
    const allData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      status: activeButton,
    };
    const userData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
      status: 'AC',
      user_id: roleName === 'ADMIN' ? null : userID,
      project_status: activeButton,
      project_manager_id: roleName === 'PROJECT MANAGER' ? true : false,
    };
    postDataForFilter(activeButton === 'ALL' ? allData : userData);
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
        <ProjectSubheader
          title="Project List"
          navigation="/home"
          description="Manage your Project List"
        />
      </div>
      <div>
        <CustomLoader
          loading={isLoading === true ? getAllLoading : FilterLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.header}>
            <div className={Styles.firstHeader}>
              {/* <div className={Styles.text}>
                <div className={Styles.textStyle}>
                  <h3>PROJECTS</h3>
                </div>
              </div> */}
              <div>
                {isProjectCreate && (
                  <div>
                    <Button
                      shape="rectangle"
                      justify="center"
                      size="small"
                      color="primary"
                      icon={<AddIcon color="white" />}
                      onClick={() => navigate('/project-add')}
                    >
                      New Project
                    </Button>
                  </div>
                )}
              </div>
            </div>
            <div className={Styles.button}>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div>
            <div className={Styles.inputFilter1}>
              <Input
                width="260px"
                prefixIcon={<SearchIcon />}
                name="search_by_name"
                value={filterValues.search_by_name}
                onChange={(e) => handleFilterChange(e)}
                placeholder="Search"
              />
            </div>
          </div>
          {/* <div className={Styles.dividerStyle}></div> */}
          {/* <div className={Styles.searchField}>
        
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
            </div> */}

          {/* <div className={Styles.dividerStyle}></div> */}
          <div className={Styles.tableContainer}>
            <div>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th className={Styles.tableHeading}>#</th>
                    <th className={Styles.tableHeading}>Name</th>
                    <th className={Styles.tableHeading}>Code</th>
                    <th className={Styles.tableHeading}>Manager</th>
                    <th className={Styles.tableHeading}>Status</th>
                    <th className={Styles.tableHeading}>Start Date</th>
                    <th className={Styles.tableHeading}>End Date</th>
                    <th className={Styles.tableHeading}>Actions</th>
                  </tr>
                </thead>
                <tbody>
                  {getFilterData?.total_count === 0 ? (
                    <tr>
                      <td colSpan="7" style={{ textAlign: 'center' }}>
                        No data found
                      </td>
                      {activeButton === 'Inprogress' && <td></td>}
                    </tr>
                  ) : (
                    ''
                  )}
                  {getFilterData?.content?.map((data: any, index: number) => {
                    return (
                      <tr key={data.project_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data?.project_name}</td>
                        <td>{data?.code}</td>
                        <td>
                          {data?.user?.first_name} {data?.user?.last_name}
                        </td>
                        <td>
                          {' '}
                          <span className={Styles.status}>{data?.status} </span>
                        </td>
                        <td>
                          {format(new Date(data?.date_started), 'MMM dd, yyyy')}
                        </td>
                        <td>
                          {format(new Date(data?.date_ended), 'MMM dd, yyyy')}
                        </td>
                        {/* {activeButton === 'AC' && ( */}
                        <td>
                          <div className={Styles.tablerow}>
                            <ViewIcon
                              onClick={() =>
                                navigate(`/project-info/${data?.project_id}`)
                              }
                            />
                            <StoreIcon
                              onClick={() =>
                                navigate(
                                  `/project-inventory/${data?.project_id}`
                                )
                              }
                            />
                            {isProjectEdit && (
                              <EditIcon
                                onClick={() =>
                                  navigate(`/project-edit/${data?.project_id}`)
                                }
                              />
                            )}

                            {/* <DeleteIcon
                            onClick={() =>
                              deleteProjectHandler(data.project_id)
                            }
                          /> */}
                          </div>
                        </td>
                        {/* )} */}
                      </tr>
                    );
                  })}
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
