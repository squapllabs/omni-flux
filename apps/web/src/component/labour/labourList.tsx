import React, { useState, useEffect } from 'react';
import Styles from '../../styles/labourList.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import EditIcon from '../menu/icons/editIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import SearchIcon from '../menu/icons/search';
import { useNavigate } from 'react-router-dom';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomDelete from '../ui/customDeleteDialogBox';
import {
  getBySearchLabour,
  useDeleteLabour,
  useGetAllLabour,
} from '../../hooks/labour-hooks';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import { formatBudgetValue } from '../../helper/common-function';

const LabourList = () => {
  const [initialValues, setInitialValues] = useState({
    labour_id: '',
    labour_type: '',
    uom_id: '',
    rate: '',
  });
  const navigate = useNavigate();
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [filter, setFilter] = useState(false);
  const [isLoading, setIsLoading] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const [filterValues, setFilterValues] = useState({
    global_search: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getBySearchLabour();

  const object: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    global_search: filterValues.global_search,
  };
  const {
    isLoading: getAllLoadingLabourData,
    data: initialData,
    refetch,
  } = useGetAllLabour(object);
  const { mutate: getDeleteLabourByID } = useDeleteLabour();

  /* Function for search */
  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      ...filterValues,
    };
    postDataForFilter(demo);
    setDataShow(true);
    setIsLoading(false);
    setFilter(true);
  };

  /* Function for resting the search field and data to normal state */
  const handleReset = async () => {
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
    setFilterValues({
      global_search: '',
    });
    setIsLoading(false);
    setDataShow(false);
    setIsResetDisabled(true);
  };

  /* Function for Filter Change */
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;

    setFilterValues({
      ...filterValues,
      ['global_search']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      handleReset();
    }
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

  /* Function for group button (Active and Inactive status) */
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  /* Function for editing the Category */
  const handleEdit = (id: any) => {
    navigate(`/labour-edit/${id}`);
  };

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

  const deleteLabourHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };

  /* Function for closing the delete popup */
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  /* Function for deleting a category */
  const deleteLabour = () => {
    getDeleteLabourByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
    // handleSearch();
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader
        loading={FilterLoading ? FilterLoading : getAllLoadingLabourData}
        size={48}
        color="#333C44"
      >
        <div>
          <div className={Styles.top}>
            <div className={Styles.textContent}>
              <h3>Add New Labour</h3>
            </div>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon color="white" />}
                onClick={() => {
                  navigate('/labour-add');
                }}
              >
                Add Labour
              </Button>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.box}>
            <div className={Styles.textContent1}>
              <h3>List of Labour Data</h3>
            </div>
            <div className={Styles.searchField}>
              <div className={Styles.inputFilter}>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="global_search"
                  value={filterValues.global_search}
                  onChange={(e) => handleFilterChange(e)}
                  placeholder="Search by labour type"
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
                  disabled={isResetDisabled}
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
                      <th>S No</th>
                      <th>Labour Type</th>
                      <th>UOM Type</th>
                      <th>Rate</th>
                      {activeButton === 'AC' && <th>Actions</th>}
                    </tr>
                  </thead>
                  <tbody>
                    {dataShow ? (
                      getFilterData?.total_count === 0 ? (
                        <tr>
                          <td></td>
                          <td></td>
                          <td>No data found</td>
                          {activeButton === 'AC' && <td></td>}
                        </tr>
                      ) : (
                        getFilterData?.content?.map(
                          (data: any, index: number) => (
                            <tr key={data.labour_id}>
                              <td>{startingIndex + index}</td>
                              <td>{data.labour_type}</td>
                              <td>{data.uom?.name}</td>
                              <td>{formatBudgetValue(data.rate)}</td>
                              {activeButton === 'AC' && (
                                <td>
                                  <div className={Styles.tableIcon}>
                                    <EditIcon
                                      onClick={() => handleEdit(data.labour_id)}
                                    />
                                    <DeleteIcon
                                      onClick={() =>
                                        deleteLabourHandler(data.labour_id)
                                      }
                                    />
                                  </div>
                                </td>
                              )}
                            </tr>
                          )
                        )
                      )
                    ) : initialData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td>No data found</td>
                        {activeButton === 'AC' && <td></td>}
                      </tr>
                    ) : (
                      initialData?.content?.map((item: any, index: number) => (
                        <tr key={item.labour_id}>
                          <td>{startingIndex + index}</td>
                          <td>{item.labour_type}</td>
                          <td>{item.uom?.name}</td>
                          <td>{formatBudgetValue(item.rate)}</td>

                          {activeButton === 'AC' && (
                            <td>
                              <div className={Styles.tableIcon}>
                                <div>
                                  <EditIcon
                                    onClick={() => handleEdit(item.labour_id)}
                                  />
                                </div>
                                <div>
                                  <DeleteIcon
                                    onClick={() =>
                                      deleteLabourHandler(item.labour_id)
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
                    dataShow
                      ? getFilterData?.total_page
                      : initialData?.total_page
                  }
                  totalCount={
                    dataShow
                      ? getFilterData?.total_count
                      : initialData?.total_count
                  }
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        </div>
      </CustomLoader>
      <CustomDelete
        open={openDelete}
        title="Delete Labour"
        contentLine1="Are you sure you want to delete this Labour ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteLabour}
      />
    </div>
  );
};

export default LabourList;
