import React, { useState, useEffect } from 'react';
import Styles from '../../styles/masterdata.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import {
  useDeletemasertData,
  useGetAllPaginatedMasterData,
} from '../../hooks/masertData-hook';
import EditIcon from '../menu/icons/newEditIcon';
import SearchIcon from '../menu/icons/search';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomLoader from '../ui/customLoader';
import AddIcon from '../menu/icons/addIcon';
import CustomPagination from '../menu/CustomPagination';
import CustomSidePopup from '../ui/CustomSidePopup';
import MasterDataForm from './masterDataForm';
import MasterDataIcon from '../menu/icons/masterDataIcon';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import {handleSortByColumn} from './../../helper/common-function'

const MaterData = () => {
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [value, setValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [masterDataID, setMasterDataID] = useState();
  const [mode, setMode] = useState('');
  const activeButton = 'AC';
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [reload, setReload] = useState(false);
  const [masterDataFormOpen, setMasterDataFormOpen] = useState(false);
  const [sortColumn, setSortColumn] = useState('');
  const [sortOrder, setSortOrder] = useState('desc');

  const { mutate: getDeleteMasterDataID } = useDeletemasertData();

  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: sortColumn === '' ? 'created_date' : sortColumn,
    order_by_direction: sortOrder,
    status: activeButton,
    global_search: filterValues?.search_by_name,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedMasterData(masterData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, sortColumn, sortOrder]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

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
      <CustomLoader loading={getAllLoadingPaginated} size={48} color="#333C44">
        {initialData?.is_available ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.subHeading}>
                  <MasterDataIcon />
                  <h4>MASTER DATA</h4>
                </div>
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
                </div>
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
                      search_by_name: e.target.value,
                    });
                    setCurrentPage(1);
                  }}
                />
              </div>
            </div>
            <div className={Styles.box}>
              <div className={Styles.tableContainer}>
                <div>
                  <table className={Styles.scrollable_table}>
                    <thead>
                      <tr>
                        <th>#</th>
                        <th
                          onClick={() =>  handleSortByColumn('master_data_name',sortOrder,setSortOrder,setSortColumn)}
                        >
                          <div className={Styles.headingRow}>
                            <div>Name</div>
                            <div>
                              <FilterOrderIcon />
                            </div>
                          </div>
                        </th>
                        <th
                          onClick={() =>  handleSortByColumn('master_data_type',sortOrder,setSortOrder,setSortColumn)}
                        >
                          <div className={Styles.headingRow}>
                            <div>Code</div>
                            <div>
                              <FilterOrderIcon />
                            </div>
                          </div>
                        </th>
                        <th>Parent Type</th>
                        {activeButton === 'AC' ? <th>Action</th> : ''}
                      </tr>
                    </thead>
                    <tbody>
                      {initialData?.count === 0 ? (
                        <tr>
                          <td></td>
                          <td></td>
                          <td></td>
                          <td>No data found</td>
                          {activeButton === 'AC' && <td></td>}
                        </tr>
                      ) : (
                        initialData?.content?.map(
                          (data: any, index: number) => (
                            <tr key={data.uom_id}>
                              <td>{startingIndex + index}</td>
                              <td>{data.master_data_name}</td>
                              <td>{data.master_data_type}</td>
                              <td>
                                {data?.parent?.master_data_name === undefined
                                  ? '-'
                                  : data?.parent?.master_data_name}
                              </td>
                              {activeButton === 'AC' && (
                                <td className={Styles.tablerow}>
                                  <EditIcon
                                    onClick={() =>
                                      handleEdit(data.master_data_id)
                                    }
                                  />
                                </td>
                              )}
                            </tr>
                          )
                        )
                      )}
                    </tbody>
                  </table>
                </div>
              </div>
              <div className={Styles.pagination}>
                <CustomPagination
                  currentPage={currentPage}
                  totalPages={initialData?.total_page}
                  totalCount={initialData?.count}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        ) : (
          <div>
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
                <span className={Styles.spanContent}>
                  Go ahead, add a Master Data
                </span>
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
