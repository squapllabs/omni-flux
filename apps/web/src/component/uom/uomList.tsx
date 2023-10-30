import React, { useState, useEffect } from 'react';
import Styles from '../../styles/uom.module.scss';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import EditIcon from '../menu/icons/newEditIcon';
import CustomSnackBar from '../ui/customSnackBar';
import {
  useDeleteUom,
  getByUom,
  useGetAllPaginatedUomData,
} from '../../hooks/uom-hooks';
import UomForm from './uomForm';
import Button from '../ui/Button';
import Input from '../ui/Input';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/CustomPagination';
import SearchIcon from '../menu/icons/search';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSidePopup from '../ui/CustomSidePopup';

/* Function for Unit of Measurement */
const UomList = () => {
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getByUom();

  const { mutate: getDeleteuomByID } = useDeleteUom();
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [uomId, setUomID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [value, setValue] = useState();
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const [openUomForm, setOpenUomForm] = useState(false);

  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const uomData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    global_search: filterValues.search_by_name,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedUomData(uomData);

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  /* Function for editing a data in the UOM List */
  const handleEdit = (value: any) => {
    setMode('EDIT');
    setUomID(value);
    setOpenUomForm(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  /* Function for deleting a data from the UOM List */
  const deleteUom = () => {
    getDeleteuomByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
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
    // handleSearch();
    refetch();
  }, [currentPage, rowsPerPage]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  /* Function for searching data in the UOM Table */
  const handleSearch = async () => {
    const uomData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_by',
      order_by_direction: 'desc',
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(uomData);
    setDataShow(true);
    setIsLoading(false);
    setFilter(true);
  };
  /* Function for resting the table to its actual state after search */
  const handleReset = async () => {
    setDataShow(false);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsResetDisabled(true);
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

  const handleCloseUomForm = () => {
    setOpenUomForm(false);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div>
      <div>
        <CustomLoader
          loading={searchLoader ? searchLoader : getAllLoadingPaginated}
          size={48}
          color="#333C44"
        >
          {initialData?.is_available ? (
            <div>
              <div className={Styles.topHeading}>
                <div className={Styles.heading}>
                  <div className={Styles.subHeading}>
                    <h3>UOM</h3>
                  </div>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon color="white" />}
                      onClick={() => {
                        setMode('ADD');
                        setOpenUomForm(true);
                      }}
                    >
                      Add Uom
                    </Button>
                  </div>
                </div>
                <div className={Styles.searchBar}>
                  <div className={Styles.searchFeild}>
                    <Input
                      placeholder="Search UOM"
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
                </div>
              </div>
              <div className={Styles.box}>
                {/* <div className={Styles.textContent}>
                  <h3>List of Unit of Measurements</h3>
                  <span className={Styles.content}>
                    Manage your UOM details here.
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
                </div> */}
                <div className={Styles.tableContainer}>
                  <div>
                    <table className={Styles.scrollable_table}>
                      <thead>
                        <tr>
                          <th>#</th>
                          <th>UOM Name</th>
                          <th>Description</th>
                          <th>Actions</th>
                        </tr>
                      </thead>
                      <tbody>
                        {dataShow ? (
                          getFilterData?.total_count === 0 ? (
                            <tr>
                              <td></td>
                              <td></td>
                              <td>No data found</td>
                              <td></td>
                            </tr>
                          ) : (
                            getFilterData?.content?.map(
                              (data: any, index: number) => (
                                <tr key={data.uom_id}>
                                  <td>{startingIndex + index}</td>
                                  <td>{data.name}</td>
                                  <td>{data.description}</td>

                                  <td>
                                    <div className={Styles.tablerow}>
                                      <EditIcon
                                        onClick={() => handleEdit(data.uom_id)}
                                      />
                                      {/* <DeleteIcon
                                        onClick={() =>
                                          deleteCategoryHandler(data.uom_id)
                                        }
                                      /> */}
                                    </div>
                                  </td>
                                </tr>
                              )
                            )
                          )
                        ) : initialData?.total_count === 0 ? (
                          <tr>
                            <td></td>
                            <td></td>
                            <td>No data found</td>
                            <td></td>
                          </tr>
                        ) : (
                          initialData?.content?.map((data: any, index: number) => (
                            <tr key={data.uom_id}>
                              <td>{startingIndex + index}</td>
                              <td>{data.name}</td>
                              <td>{data.description}</td>

                              <td>
                                <div className={Styles.tablerow}>
                                  <EditIcon
                                    onClick={() => handleEdit(data.uom_id)}
                                  />
                                  {/* <DeleteIcon
                                    onClick={() =>
                                      deleteCategoryHandler(data.uom_id)
                                    }
                                  /> */}
                                </div>
                              </td>
                            </tr>
                          ))
                        )}
                      </tbody>
                    </table>
                  </div>
                </div>
                <div className={Styles.pagination}>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={
                      dataShow ? getFilterData?.total_page : initialData?.total_page
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
          ) : (
            <div>
            <div className={Styles.subHeading}>
              {/* <span>UOM</span> */}
            </div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/UOM.jpg"
                  alt="aa"
                  width="100%"
                  height="200px"
                />
              </div>
              <div>
                <h5>UOM is Empty</h5>
              </div>
              <div className={Styles.contentGap}>
                <span className={Styles.spanContent}>Go ahead, add new Unit of Measure</span>
              </div>
              <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon color="white" />}
                      onClick={() => {
                        setMode('ADD');
                        setOpenUomForm(true);
                      }}
                    >
                      Add Uom
                    </Button>
              </div>
            </div>
          </div>
          )}

        </CustomLoader>
        <CustomSidePopup
          open={openUomForm}
          title={mode === 'EDIT' ? 'Edit UOM' : 'Add UOM'}
          handleClose={handleCloseUomForm}
          content={
            <UomForm
              open={openUomForm}
              setOpen={setOpenUomForm}
              reload={reload}
              setReload={setReload}
              openSnack={openSnack}
              setOpenSnack={setOpenSnack}
              message={message}
              setMessage={setMessage}
              mode={mode}
              uomId={uomId}
            />
          }
        />
        <CustomDelete
          open={openDelete}
          title="Delete UOM"
          contentLine1="Are you sure you want to delete this UOM Data"
          contentLine2=""
          handleClose={handleCloseDelete}
          handleConfirm={deleteUom}
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </div>
    </div>
  );
};

export default UomList;
