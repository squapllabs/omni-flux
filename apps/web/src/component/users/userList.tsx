import React, { useState, useEffect } from 'react';
import Styles from '../../styles/userList.module.scss';
import {
  useGetAllUsers,
  useDeleteUsers,
  getByUser,
  useGetAllPaginatedUser,
} from '../../hooks/user-hooks';
import { useNavigate } from 'react-router';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import Button from '../ui/Button';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomLoader from '../ui/customLoader';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import Pagination from '../menu/CustomPagination';
import EditIcon from '../menu/icons/newEditIcon';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import AddIcon from '../menu/icons/addIcon';
import MemberIcon from '../menu/icons/memberIcon';

/* Function for User List */
const UserList = () => {
  // const { isLoading: getAllLoading } = useGetAllUsers();
  const { mutate: getDeleteUserByID } = useDeleteUsers();
  // const {
  //   mutate: postDataForFilter,
  //   data: getFilterData,
  //   isLoading: FilterLoading,
  // } = getByUser();
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [isLoading, setIsLoading] = useState(true);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [filter, setFilter] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const navigate = useNavigate();
  const userData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_by',
    order_by_direction: 'desc',
    global_search: filterValues?.search_by_name,
    status: activeButton,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedUser(userData);

  const deleteUserHandler = (id: any) => {
    setValue(id);
    setOpen(true);
  };
  const handleClose = () => {
    setOpen(false);
  };
  const handleSnackBarClose = () => {
    setOpenDeleteSnack(false);
  };

  /* Function for deleting a user data */
  const deleteUser = () => {
    getDeleteUserByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenDeleteSnack(true);
  };

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

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

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);
  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 2000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  /* Function for searching a user in the table */
  // const handleSearch = async () => {
  //   const userData: any = {
  //     limit: rowsPerPage,
  //     offset: (currentPage - 1) * rowsPerPage,
  //     order_by_column: 'updated_date',
  //     order_by_direction: 'desc',
  //     global_search: filterValues.search_by_name,
  //     status: activeButton,
  //   };
  //   setTimeout(() => {
  //     postDataForFilter(userData);
  //   }, 2000);
  //   // setDataShow(true);
  //   // setIsLoading(false);
  //   // setFilter(true);
  // };

  /* Function for reseting the table to its actual state after search */
  const handleReset = async () => {
    setDataShow(false);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
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
  // const handleChange = (e: any) => {
  //   console.log(e.target.value);
  //   const searchValue = e.target.value;
  //   setFilterValues({
  //     ...filterValues,
  //     ['search_by_name']: e.target.value,
  //   });
  //   handleSearch();
  // };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div className={Styles.container}>
      <div>
        <CustomLoader
          loading={getAllLoadingPaginated}
          size={48}
          color="#333C44"
        >
          <div className={Styles.topHeading}>
            <div className={Styles.heading}>
              <div className={Styles.subHeading}>
                <MemberIcon />
                <h3>USERS</h3>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => navigate('/user-create')}
                >
                  Add User
                </Button>
              </div>
            </div>
            <div className={Styles.filters}>
              <div>
                <Input
                  placeholder="Search Users"
                  width="300px"
                  prefixIcon={<SearchIcon />}
                  name="filter_value"
                  onChange={(e) => {
                    setFilterValues({
                      ...filterValues,
                      ['search_by_name']: e.target.value,
                    });
                  }}
                />
              </div>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>First Name</th>
                    <th>Last Name</th>
                    <th>Email</th>
                    <th>Contact Number</th>
                    {activeButton === 'AC' && <th>Actions</th>}
                  </tr>
                </thead>
                <tbody>
                  {initialData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td></td>
                      <td>No data found</td>
                      {activeButton === 'AC' && <td></td>}
                    </tr>
                  ) : (
                    initialData?.content?.map((data: any, index: number) => (
                      <tr key={data.user_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data.first_name}</td>
                        <td>{data.last_name}</td>
                        <td>{data.email_id}</td>
                        <td>{data.contact_no}</td>
                        {activeButton === 'AC' && (
                          <td>
                            <div className={Styles.tablerow}>
                              <EditIcon
                                onClick={() =>
                                  navigate(`/user-edit/${data.user_id}`)
                                }
                              />
                              <DeleteIcon
                                onClick={() => deleteUserHandler(data.user_id)}
                              />
                            </div>
                          </td>
                        )}
                      </tr>
                    ))
                  )}
                </tbody>
              </table>
            </div>
            <div className={Styles.pagination}>
              <Pagination
                currentPage={currentPage}
                totalPages={initialData?.total_page}
                totalCount={initialData?.total_count}
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
          title="Delete User"
          contentLine1="Are you sure you want to delete this User?"
          contentLine2=""
          handleConfirm={deleteUser}
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

export default UserList;
