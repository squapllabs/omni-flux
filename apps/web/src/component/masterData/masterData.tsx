import React, { useState, useEffect } from 'react';
import Styles from '../../styles/masterdata.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import {
  useGetAllmasertData,
  useGetAllParentmasertDataDrop,
  useDeletemasertData,
  getBySearchmasterData,
  useGetAllPaginatedMasterData,
} from '../../hooks/masertData-hook';
import EditIcon from '../menu/icons/newEditIcon';
import SearchIcon from '../menu/icons/search';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomLoader from '../ui/customLoader';
import AddIcon from '../menu/icons/addIcon';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import CustomPagination from '../menu/CustomPagination';
import CustomSidePopup from '../ui/CustomSidePopup';
import MasterDataForm from './masterDataForm';
import MasterDataIcon from '../menu/icons/masterDataIcon';

const MaterData = () => {
  const [selectedValue, setSelectedValue] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [value, setValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [masterDataID, setMasterDataID] = useState();
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(3);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [reload, setReload] = useState(false);
  const [masterDataFormOpen, setMasterDataFormOpen] = useState(false);
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getBySearchmasterData();

  const { data: getAllmasterData, isLoading: getAllloading } =
    useGetAllmasertData();
  const { data: getAllmasterDataForDrop = [], isLoading: dropLoading } =
    useGetAllParentmasertDataDrop();
  const { mutate: getDeleteMasterDataID } = useDeletemasertData();
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      // handleReset();
    }
  };
  const [dataShow, setDataShow] = useState(false);
  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    global_search: filterValues?.search_by_name,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedMasterData(masterData);

  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
    setIsResetDisabled(searchValue === '');
  };
  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage]);
  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);
  // const handleSearch = async () => {
  //   const masterData: any = {
  //     offset: (currentPage - 1) * rowsPerPage,
  //     limit: rowsPerPage,
  //     order_by_column: 'updated_date',
  //     order_by_direction: 'asc',
  //     status: activeButton,
  //     global_search: filterValues.search_by_name,
  //     parent_id: Number(selectedValue),
  //   };
  //   postDataForFilter(masterData);
  //   setDataShow(true);
  //   setIsLoading(false);
  //   setFilter(true);
  // };
  // const handleReset = async () => {
  //   setFilterValues({
  //     search_by_name: '',
  //   });
  //   setSelectedValue('');
  //   setDataShow(false);
  //   setIsLoading(false);
  //   setFilter(false);
  //   setIsLoading(false);
  //   setSelectedValue('');
  //   setIsResetDisabled(true);
  // };
  // const handlePageChange = (page: React.SetStateAction<number>) => {
  //   setCurrentPage(page);
  // };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  const handleEdit = (value: any) => {
    setMode('Edit');
    setMasterDataID(value);
    setMasterDataFormOpen(true);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const deleteCategory = () => {
    getDeleteMasterDataID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  const handleCloseMasterForm = () => {
    setMasterDataFormOpen(false);
  };
  return (
    <div>
      <CustomLoader
        loading={getAllLoadingPaginated}
        size={48}
        color="#333C44"
      >

        <div className={Styles.topHeading}>
          <div className={Styles.heading}>
            <div className={Styles.subHeading}>
              <MasterDataIcon />
              <h4>MASTER DATA</h4>
            </div>
            {initialData?.total_count !== 0 ? (
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => {
                    setMode('Add');
                    setMasterDataFormOpen(true);
                  }}
                >
                  Add Master Data
                </Button>
              </div> ) : 
             ''}
          </div>
          <div className={Styles.searchBar}>
            <Input
              placeholder="Search Data"
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
        </div>
        {/* <div className={Styles.dividerStyle}></div> */}
        {initialData?.total_count !== 0 ? (
          <div className={Styles.box}>
            {/* <div className={Styles.textContent}>
                <h3>List of Master Data</h3>
                <span className={Styles.content}>
                  List of all existing master data entries
                </span>
              </div> */}
            {/* <div className={Styles.searchField}>
                <div className={Styles.inputFilter}>
                  <Input
                    width="260px"
                    prefixIcon={<SearchIcon />}
                    name="search_by_name"
                    value={filterValues.search_by_name}
                    onChange={(e) => handleFilterChange(e)}
                    placeholder="Search by name"
                  />
                  <AutoCompleteSelect
                    name="parent_master_data_id"
                    defaultLabel="Select Parent Name"
                    onChange={() => handleDropdownChange}
                    value={selectedValue}
                    placeholder="Parent Name"
                    width="260px"
                    onSelect={(value) => {
                      setSelectedValue(value);
                      setIsResetDisabled(false);
                    }}
                    optionList={
                      dropLoading === true ? [] : getAllmasterDataForDrop
                    }
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
              </div> */}
            <div className={Styles.tableContainer}>
              <div>
                <table className={Styles.scrollable_table}>
                  <thead>
                    <tr>
                      <th>#</th>
                      <th>Name</th>
                      <th>Description</th>
                      <th>Code</th>
                      <th>Parent Name</th>
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
                        <tr key={data.uom_id}>
                          <td>{startingIndex + index}</td>
                          <td>{data.master_data_name}</td>
                          <td>
                            <span
                              title={data?.master_data_description}
                              className={Styles.truncatedStyle}
                            >
                              {data.master_data_description
                                ? data.master_data_description.length > 20
                                  ? data.master_data_description.substring(
                                      0,
                                      20
                                    ) + '...'
                                  : data.master_data_description
                                : '-'}
                            </span>
                          </td>
                          <td>{data.master_data_type}</td>
                          <td>
                            {data?.parent?.master_data_name === undefined
                              ? '-'
                              : data?.parent?.master_data_name}
                          </td>
                          {activeButton === 'AC' && (
                            <td className={Styles.tablerow}>
                              <EditIcon
                                onClick={() => handleEdit(data.master_data_id)}
                              />
                            </td>
                          )}
                        </tr>
                      ))
                    )}
                  </tbody>
                </table>
              </div>
            </div>
            <div className={Styles.pagination}>
              <CustomPagination
                currentPage={currentPage}
                totalPages={initialData?.total_page}
                totalCount={initialData?.total_count}
                rowsPerPage={rowsPerPage}
                onPageChange={handlePageChange}
                onRowsPerPageChange={handleRowsPerPageChange}
              />
            </div>
          </div>

        ) : (
          <div>
            {/* <div className={Styles.subHeading}>
              <MasterDataIcon />
              <span>MASTER DATA</span>
            </div> */}
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/masterData.jpg"
                  alt="masterData_img"
                  width="100%"
                  height="250px"
                />
              </div>
              <div>
                <h5>Master Data is Empty</h5>
              </div>
              <div>
                <span className={Styles.spanContent}>Go ahead, add a Master Data</span>
              </div>
              <div className={Styles.emptyButton}>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => {
                    setMode('Add');
                    setMasterDataFormOpen(true);
                  }}
                >
                  Add Master Data
                </Button>
              </div>
            </div>
          </div>
        )}
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomDelete
          open={openDelete}
          handleClose={handleCloseDelete}
          title="Delete Category"
          contentLine1="Are you want to delete this Category?"
          contentLine2=""
          handleConfirm={deleteCategory}
        />
      </CustomLoader>
      <CustomSidePopup
        open={masterDataFormOpen}
        title={mode === 'Edit' ? 'Edit Master Data' : 'Add Master Data'}
        description={
          mode === 'Edit'
            ? 'Modify existing master data'
            : 'Create new master data'
        }
        handleClose={handleCloseMasterForm}
        content={
          <MasterDataForm
            open={masterDataFormOpen}
            setOpen={setMasterDataFormOpen}
            reload={reload}
            setReload={setReload}
            openSnack={openSnack}
            setOpenSnack={setOpenSnack}
            message={message}
            setMessage={setMessage}
            mode={mode}
            masterID={masterDataID}
          />
        }
      />
    </div>
  );
};

export default MaterData;
