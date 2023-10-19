import React, { useState, useEffect } from 'react';
import Styles from '../../styles/machinery.module.scss';
import { useNavigate } from 'react-router';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import Button from '../ui/Button';
import CustomLoader from '../ui/customLoader';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import Pagination from '../menu/CustomPagination';
import EditIcon from '../menu/icons/newEditIcon';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import AddIcon from '../menu/icons/addIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import {
  getByMachinery,
  useDeleteMachinery,
  useGetAllPaginatedMachinery,
} from '../../hooks/machinery-hooks';

const MachineryList = () => {
  // const {
  //   mutate: postDataForFilter,
  //   data: getFilterData,
  //   isLoading: FilterLoading,
  // } = getByMachinery();

  const { mutate: getDeleteMachineryByID } = useDeleteMachinery();

  const navigate = useNavigate();

  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  // const [isLoading, setIsLoading] = useState(true);
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');
  // const [filter, setFilter] = useState(false);

  const machineryData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_by',
    order_by_direction: 'desc',
    global_search: filterValues.search_by_name,
    status: activeButton,
  };

  const {
    data: getFilterData,
    isLoading: FilterLoading,  
    refetch,
  } = useGetAllPaginatedMachinery(machineryData);

  // const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
  //   const searchValue = event.target.value;
  //   setFilterValues({
  //     ...filterValues,
  //     ['search_by_name']: event.target.value,
  //   });
  //   setIsResetDisabled(searchValue === '');
  //   if (searchValue === '') {
  //     handleReset();
  //   }
  // };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

  
  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  // const handleSearch = async () => {
  //   const machineryData: any = {
  //     limit: rowsPerPage,
  //     offset: (currentPage - 1) * rowsPerPage,
  //     order_by_column: 'updated_date',
  //     order_by_direction: 'asc',
  //     global_search: filterValues.search_by_name,
  //     status: activeButton,
  //   };
  //   postDataForFilter(machineryData);
  //   // setIsLoading(false);
  //   // setFilter(true);
  // };

  // const handleReset = async () => {
  //   const machineryData: any = {
  //     limit: rowsPerPage,
  //     offset: (currentPage - 1) * rowsPerPage,
  //     order_by_column: 'updated_date',
  //     order_by_direction: 'asc',
  //     global_search: '',
  //     status: 'AC',
  //   };
  //   postDataForFilter(machineryData);

  //   setFilterValues({
  //     search_by_name: '',
  //   });
  //   // setIsLoading(false);
  //   setIsResetDisabled(true);
  // };

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const deleteMachineryHandler = (id: any) => {
    setValue(id);
    setOpen(true);
  };

  const deleteMachinery = () => {
    getDeleteMachineryByID(value);
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
        <CustomLoader loading={FilterLoading} size={48} color="#333C44">
          {/* <div className={Styles.text}>
            <div className={Styles.textStyle}>
              <h3>List of Machineries</h3>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div> */}
          {/* <div className={Styles.searchField}>
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
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon />}
                  onClick={() => navigate('/add-machinery')}
                >
                  Add
                </Button>
              </div>
            </div>
          </div> */}
          {/* <div className={Styles.dividerStyle}></div> */}
          {getFilterData?.is_available ? (
            <div>
              <div className={Styles.topHeading}>
                  <div className={Styles.heading}>
                    <div className={Styles.subHeading}>
                      <h3>MACHINERIES</h3>
                    </div>
                    <div>
                      <Button
                        color="primary"
                        shape="rectangle"
                        justify="center"
                        size="small"
                        icon={<AddIcon color="white" />}
                        onClick={() => navigate('/add-machinery')}
                      >
                        Add Machinery
                      </Button>
                    </div>
                  </div>
                  <div className={Styles.filters}>
                    <div>
                      <Input
                        placeholder="Search Machineries"
                        width="300px"
                        prefixIcon={<SearchIcon />}
                        name="filter_value"
                        onChange={(e) => {
                          setFilterValues({
                            ...filterValues,
                            ['search_by_name']: e.target.value,
                          });
                          setCurrentPage(1);
                        }}
                      />
                    </div>
                    {/* <div>
                      <CustomGroupButton
                        labels={buttonLabels}
                        onClick={handleGroupButtonClick}
                        activeButton={activeButton}
                      />
                    </div> */}
                  </div>
                </div>
              <div className={Styles.tableContainer}>
                <div>
                  <table className={Styles.scrollable_table}>
                    <thead>
                      <tr>
                        <th>#</th>
                        <th>Machinery Name</th>
                        <th>Rate</th>
                        <th>UOM</th>
                        <th>Operational Status</th>
                        {activeButton === 'AC' && <th>Options</th>}
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
                      {getFilterData?.content?.map((data: any, index: number) => (
                        <tr key={data.machinery_id}>
                          <td>{startingIndex + index}</td>
                          <td>{data.machinery_name}</td>
                          <td>{data.rate}</td>
                          <td>{data.uom_data.name}</td>
                          <td>{data.operational_status}</td>
                          {activeButton === 'AC' && (
                            <td>
                              <div className={Styles.tablerow}>
                                <EditIcon
                                  onClick={() =>
                                    navigate(`/edit-machinery/${data.machinery_id}`)
                                  }
                                />
                                {/* <DeleteIcon
                                  onClick={() =>
                                    deleteMachineryHandler(data.machinery_id)
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
            </div>
          ) : (
            <div>
            <div className={Styles.subHeading}>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/machineries_.jpg"
                  alt="aa"
                  width="100%"
                  height="300px"
                />
              </div>
              <div>
                <h5>Machineries list is Empty</h5>
              </div>
              <div>
                <span className={Styles.spanContent}>Go ahead, add new Machineries</span>
              </div>
              <div className={Styles.emptyButton}>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => navigate('/add-machinery')}
                >
                Add Machinery
                </Button>
              </div>
            </div>
          </div>
          )}
        </CustomLoader>
        <CustomDelete
          open={open}
          handleClose={handleClose}
          title="Delete Machinery"
          contentLine1="Are you want to delete this Machinery ?"
          contentLine2=""
          handleConfirm={deleteMachinery}
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

export default MachineryList;
