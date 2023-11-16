import React, { useState, useEffect } from 'react';
import { useNavigate, useLocation } from 'react-router';
import CustomDelete from '../ui/customDeleteDialogBox';
import Button from '../ui/Button';
import CustomLoader from '../ui/customLoader';
import Input from '../ui/Input';
import SearchIcon from '../menu/icons/search';
import Pagination from '../menu/CustomPagination';
import EditIcon from '../menu/icons/newEditIcon';
import ViewIcon from '../menu/icons/newViewIcon';
import AddIcon from '../menu/icons/addIcon';
import Styles from '../../styles/vendor.module.scss';
import {
  useGetAllPaginatedVendor,
  useDeleteVendor,
} from '../../hooks/vendor-hooks';
import CustomSnackbar from '../ui/customSnackBar';
import ProjectSubheader from '../project/projectSubheader';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import { handleSortByColumn } from './../../helper/common-function';

const VendorList = () => {
  const navigate = useNavigate();
  const location = useLocation();
  const currentPath = location.pathname;
  const { mutate: getDeleteVendorByID } = useDeleteVendor();
  const [activeButton, setiveButton] = useState<string | null>('AC');
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [dataShow, setDataShow] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [sortColumn, setSortColumn] = useState('');
  const [sortOrder, setSortOrder] = useState('desc');

  const vendorData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: sortColumn === '' ? 'created_date' : sortColumn,
    order_by_direction: sortOrder,
    status: activeButton,
    global_search: filterValues.search_by_name,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedVendor(vendorData);

  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const deleteVendor = () => {
    getDeleteVendorByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenDeleteSnack(true);
  };
  const handleSnackBarClose = () => {
    setOpenDeleteSnack(false);
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton, sortColumn, sortOrder]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div>
      <CustomLoader
        loading={getAllLoadingPaginated}
        size={48}
        color="#333C44"
      >
        {initialData?.is_available ? (
          <div>
            {currentPath === '/vendor-list' && (
              <div>
                <div>
                  <ProjectSubheader
                    navigation={'/home'}
                    title="Vendors"
                    description="Manage your approved vendors"
                  />
                </div>
                <div style={{ paddingTop: '10px' }}></div>
              </div>
            )}
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                {currentPath !== '/vendor-list' && (
                  <div className={Styles.subHeading}>
                    <h3>VENDORS</h3>
                  </div>
                )}
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() =>
                      navigate('/vendor-add', { state: { path: currentPath } })
                    }
                  >
                    Add Vendor
                  </Button>
                </div>
              </div>
              <div className={Styles.filters}>
                <Input
                  placeholder="Search Vendors"
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
            <div className={Styles.tableContainer}>
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>#</th>
                    <th
                      onClick={() =>
                        handleSortByColumn(
                          'vendor_name',
                          sortOrder,
                          setSortOrder,
                          setSortColumn
                        )
                      }
                    >
                      <div className={Styles.headingRow}>
                        <div>Vendor Name</div>
                        <div>
                          <FilterOrderIcon />
                        </div>
                      </div>
                    </th>
                    <th
                      onClick={() =>
                        handleSortByColumn(
                          'contact_person',
                          sortOrder,
                          setSortOrder,
                          setSortColumn
                        )
                      }
                    >
                      <div className={Styles.headingRow}>
                        <div>Contact Person Name</div>
                        <div>
                          <FilterOrderIcon />
                        </div>
                      </div>
                    </th>
                    <th>Phone Number</th>
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
                      <tr key={data.vendor_id}>
                        <td>{startingIndex + index}</td>
                        <td>{data.vendor_name}</td>
                        <td>{data.contact_person}</td>
                        <td>{data.contact_phone_no}</td>
                        {activeButton === 'AC' && (
                          <td>
                            <div className={Styles.tablerow}>
                              <EditIcon
                                onClick={() =>
                                  navigate(`/vendor-edit/${data.vendor_id}`, {
                                    state: { path: currentPath },
                                  })
                                }
                              />
                              <ViewIcon
                                onClick={() =>
                                  navigate(`/vendor-info/${data.vendor_id}`)
                                }
                              />
                            </div>
                          </td>
                        )}
                      </tr>
                    ))
                  )
                  }
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
        ) : (
          <div>
            <div className={Styles.subHeading}></div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/vendor.jpg"
                  alt="aa"
                  width="100%"
                  height="150px"
                  style={{ paddingBottom: '15px' }}
                />
              </div>
              <div>
                <h5>Vendors List is Empty</h5>
              </div>
              <div className={Styles.contentGap}>
                <span className={Styles.spanContent}>
                  Go ahead, add new vendors
                </span>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => navigate('/vendor-add')}
                >
                  Add Vendor
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
      <CustomDelete
        open={openDelete}
        title="Delete Vendor"
        contentLine1="Are you sure you want to delete this Vendor ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteVendor}
      />
      <CustomSnackbar
        open={openDeleteSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default VendorList;
