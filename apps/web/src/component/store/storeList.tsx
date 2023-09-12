import React, { useEffect, useState } from 'react';
import Styles from '../../styles/store.module.scss';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import SearchIcon from '../menu/icons/search';
import { useNavigate } from 'react-router-dom';
import AddIcon from '../menu/icons/addIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomDelete from '../ui/customDeleteDialogBox';
import EditIcon from '../menu/icons/editIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomSnackBar from '../ui/customSnackBar';
import {
  getBySearchStore,
  useDeleteStore,
  useGetAllPaginatedStoreData,
} from '../../hooks/store-hooks';

const StoreList = () => {
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getBySearchStore();
  const { mutate: getDeleteStoreByID } = useDeleteStore();
  const navigate = useNavigate();
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [dataShow, setDataShow] = useState(false);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
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

  const storeData: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    global_search: filterValues.search_by_name,
  };

  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedStoreData(storeData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

  const handleSearch = async () => {
    const storeData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(storeData);
    setDataShow(true);
  };

  const handleReset = async () => {
    setDataShow(false);
    postDataForFilter(storeData);
    setFilterValues({
      search_by_name: '',
    });
    setIsResetDisabled(true);
  };

  const handleEdit = (id: any) => {
    navigate(`/store-edit/${id}`);
  };

  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const deleteStore = () => {
    getDeleteStoreByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
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
      <CustomLoader
        loading={FilterLoading ? FilterLoading : getAllLoadingPaginated}
        size={48}
        color="#333C44"
      >
        <div>
          <div className={Styles.top}>
            <div className={Styles.textContent}>
              <h3>Add Store</h3>
            </div>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon />}
                onClick={() => {
                  navigate('/add-store');
                }}
              >
                Add Store
              </Button>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.box}>
            <div className={Styles.textContent1}>
              <h3>List of Stores</h3>
            </div>
            <div className={Styles.searchField}>
              <div className={Styles.inputFilter}>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="search_by_name"
                  value={filterValues.search_by_name}
                  onChange={(e) => handleFilterChange(e)}
                  placeholder="Search by item name"
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
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <table>
              <thead>
                <tr>
                  <th>S No</th>
                  <th>Store Name</th>
                  <th>Store Manager Name</th>
                  <th>Contact Number</th>
                  <th>Project</th>
                  {activeButton === 'AC' && <th></th>}
                </tr>
              </thead>
              <tbody>
                {dataShow ? (
                  getFilterData?.total_count === 0 ? (
                    <tr>
                      <td></td>
                      <td>No data found</td>
                      {activeButton === 'AC' && <td></td>}
                    </tr>
                  ) : (
                    getFilterData?.content?.map((item: any, index: number) => (
                      <tr key={item.store_id}>
                        <td>{startingIndex + index}</td>
                        <td>{item.store_name}</td>
                        <td>{item.store_manager_data.user_name}</td>
                        <td>{item.contact_phone}</td>
                        <td>{item.project_data.project_name}</td>
                        {activeButton === 'AC' && (
                          <td>
                            <div className={Styles.tableIcon}>
                              <div>
                                <EditIcon
                                  onClick={() => handleEdit(item.store_id)}
                                />
                              </div>
                              <div>
                                <DeleteIcon
                                  onClick={() =>
                                    deleteCategoryHandler(item.store_id)
                                  }
                                />
                              </div>
                            </div>
                          </td>
                        )}
                      </tr>
                    ))
                  )
                ) : initialData?.total_count === 0 ? (
                  <tr>
                    <td></td>
                    <td></td>
                    <td>No data found</td>
                    {activeButton === 'AC' && <td></td>}
                  </tr>
                ) : (
                  initialData?.content?.map((item: any, index: number) => (
                    <tr key={item.store_id}>
                      <td>{startingIndex + index}</td>
                      <td>{item.store_name}</td>
                      <td>{item.store_manager_data.user_name}</td>
                      <td>{item.contact_phone}</td>
                      <td>{item.project_data.project_name}</td>
                      {activeButton === 'AC' && (
                        <td>
                          <div className={Styles.tableIcon}>
                            <div>
                              <EditIcon
                                onClick={() => handleEdit(item.store_id)}
                              />
                            </div>
                            <div>
                              <DeleteIcon
                                onClick={() =>
                                  deleteCategoryHandler(item.store_id)
                                }
                              />
                            </div>
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
              totalPages={
                dataShow ? getFilterData?.total_page : initialData?.total_page
              }
              totalCount={
                dataShow ? getFilterData?.total_count : initialData?.total_count
              }
              rowsPerPage={rowsPerPage}
              onPageChange={handlePageChange}
              onRowsPerPageChange={handleRowsPerPageChange}
            />
          </div>
        </div>
      </CustomLoader>
      <CustomDelete
        open={openDelete}
        title="Delete Store"
        contentLine1="Are you sure you want to delete this store ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteStore}
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};
export default StoreList;
