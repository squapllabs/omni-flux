import React, { useState,useEffect } from 'react';
import Styles from '../../styles/userList.module.scss';
import { useGetAllUsers, useDeleteUsers, getByUser } from '../../hooks/user-hooks';
import { useNavigate } from 'react-router';
import CustomDialog from '../ui/customDialog';
import MySnackbar from '../ui/MySnackbar';
import Button1 from '../menu/button';
import Button from '../ui/Button';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomLoader from '../ui/customLoader';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import Pagination from '../menu/pagination';
import EditIcon from '../menu/icons/editIcon';

const UserList = () => {
  const { isLoading: getAllLoading } = useGetAllUsers();


  const { mutate: getDeleteUserByID } = useDeleteUsers();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getByUser();
  console.log("data>>>>>>>",getFilterData);
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
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const navigate = useNavigate();

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
      page: 0,
      size: 5,
      sort: 'asc',
      global_filter: filterValues.search_by_name,
      status: activeButton,
    };
    console.log('demo',demo);
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(true);
    console.log();
    
  };

  const handleReset = async () => {
    const demo: any = {
      page: 0,
      size: 5,
      sort: 'asc',
      status: 'AC',
      global_filter: '',
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
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
    <div className={Styles.container}>
      <div>
        <CustomLoader
          loading={isLoading === true ? getAllLoading : FilterLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.textContent}>
            <div className={Styles.textStyle}>
              <h3>List of Users</h3>
            </div>
            <div className={Styles.buttonStyle}>
              <Button1
                text="Add"
                backgroundColor="#7F56D9"
                fontSize={14}
                fontWeight={500}
                width={100}
                onClick={() => navigate('/user-create')}
              />
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
              >
                Reset
              </Button>
            </div>
            <div>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <div>
              <table>
                <thead>
                  <tr>
                    <th>S. No</th>
                    <th>First Name</th>
                    <th>Last Name</th>
                    <th>Email</th>
                    <th>Contact Number</th>
                    <th>Options</th>
                  </tr>
                </thead>
                <tbody>
                  {getFilterData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td></td>
                      <td>No data found</td>
                      <td></td>
                    </tr>
                  ) : (
                    ''
                  )}
                  {getFilterData?.data?.content?.map((data: any, index: number) => (
                    <tr>
                      <td>{index + 1}</td>
                      <td>{data.first_name}</td>
                      <td>{data.last_name}</td>
                      <td>{data.email_id}</td>
                      <td>{data.contact_no}</td>
                      <td>
                        {/* <EditIcon onC */}
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
        </CustomLoader>
        <CustomDialog
          open={open}
          handleClose={handleClose}
          title="Delete User"
          content="Are you want to delete this User?"
          handleConfirm={deleteUser}
        />
        <MySnackbar
          open={openDeleteSnack}
          message={message}
          onClose={handleSnackBarClose}
          severity={'success'}
          autoHideDuration={1000}
        />
      </div>
    </div>
  );
};

export default UserList;
