import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectlist.module.scss';
import {
  useDeleteProjects,
  useGetPaginatedMemberBasedProject,
} from '../../hooks/project-hooks';
import Input from '../ui/Input';
import Button from '../ui/Button';
import SearchIcon from '../menu/icons/search';
import AddIcon from '../menu/icons/addIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import { format } from 'date-fns';
import EditIcon from '../menu/icons/newEditIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import { useNavigate } from 'react-router-dom';
import CustomLoader from '../ui/customLoader';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
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
    roleName === 'PLANNING ENGINEER' ||
    roleName === 'SITE ENGINEER' ||
    roleName === 'PURCHASE MANAGER';
  const isFinanceManagerLogin = roleName === 'FINANCE MANAGER';
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
  const [activeButton, setActiveButton] = useState<string | null>('ALL');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');
  const navigate = useNavigate();

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
    setCurrentPage(1);
  };

  const {
    isFetched: getAllLoadingPaginated,
    data: getFilterData,
    refetch,
  } = useGetPaginatedMemberBasedProject(
    activeButton === 'ALL'
      ? {
          limit: rowsPerPage,
          offset: (currentPage - 1) * rowsPerPage,
          order_by_column: 'updated_date',
          order_by_direction: 'desc',
          global_search: filterValues?.search_by_name,
          status: activeButton,
        }
      : {
          limit: rowsPerPage,
          offset: (currentPage - 1) * rowsPerPage,
          order_by_column: 'updated_date',
          order_by_direction: 'desc',
          global_search: filterValues?.search_by_name,
          status: 'AC',
          user_id: roleName === 'ADMIN' ? null : userID,
          project_status: activeButton,
          project_manager_id: roleName === 'PROJECT MANAGER' ? true : false,
        }
  );

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

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
          loading={!getAllLoadingPaginated}
          size={48}
          color="#333C44"
        >
          <div className={Styles.header}>
            <div className={Styles.firstHeader}>
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
                onChange={(e) => {
                  setFilterValues({
                    ...filterValues,
                    [filterValues?.search_by_name]: e.target.value,
                  });
                  setCurrentPage(1);
                }}
                placeholder="Search"
              />
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>Name</th>
                    <th>Code</th>
                    <th>Manager</th>
                    <th>Status</th>
                    <th>Start Date</th>
                    <th>End Date</th>
                    {!isFinanceManagerLogin && <th>Actions</th>}
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
                    getFilterData?.content?.map((data: any, index: number) => {
                      return (
                        <tr key={data.project_id}>
                          <td>{startingIndex + index}</td>
                          <td>{data?.project_name}</td>
                          <td>{data?.code}</td>
                          <td>
                            {data?.user?.first_name} {data?.user?.last_name}
                          </td>
                          <td>
                            <span
                              className={`${Styles.status} ${
                                data?.status === 'Inprogress'
                                  ? Styles.inprogressStatus
                                  : data?.status === 'Completed'
                                  ? Styles.completedStatus
                                  : data?.status === 'Draft'
                                  ? Styles.draftStatus
                                  : ''
                              }`}
                            >
                              {data?.status}
                            </span>
                          </td>
                          <td>
                            {format(
                              new Date(data?.date_started),
                              'MMM dd, yyyy'
                            )}
                          </td>
                          <td>
                            {format(new Date(data?.date_ended), 'MMM dd, yyyy')}
                          </td>
                          {!isFinanceManagerLogin && (
                            <td>
                              <div className={Styles.tablerow}>
                                {isProjectEdit && (
                                  <EditIcon
                                    onClick={() => {
                                      if (data?.status === 'Draft') {
                                        navigate(
                                          `/project-edit-draft/${data?.project_id}`
                                        );
                                      } else {
                                        navigate(
                                          `/project-edit/${data?.project_id}`
                                        );
                                      }
                                    }}
                                  />
                                )}
                              </div>
                            </td>
                          )}
                        </tr>
                      );
                    })
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
