import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectWorkBreakDownList.module.scss';
import SearchIcon from '../menu/icons/search';
import Input from '../ui/Input';
import Button from '../ui/Button';
import CustomLoader from '../ui/customLoader';
import {
  useDeleteSite,
  useGetAllPaginatedContractorsData,
} from '../../hooks/site-hooks';
import AddIcon from '../menu/icons/addIcon';
import { useNavigate } from 'react-router';
import EditIcon from '../menu/icons/newEditIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import CustomPagination from '../menu/CustomPagination';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import { handleSortByColumn } from './../../helper/common-function'

/* FFunction to list contractors */
const ContractorList = () => {
  const { mutate: getDeleteContractorById } = useDeleteSite();
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [currentPage, setCurrentPage] = useState(1);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [sortColumn, setSortColumn] = useState('');
  const [sortOrder, setSortOrder] = useState('desc');
  const navigate = useNavigate();

  const contractorData: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: sortColumn === '' ? 'created_date' : sortColumn,
    order_by_direction: sortOrder,
    status: activeButton,
    global_search: filterValues.search_by_name,
    type: 'Contractor',
  };

  /* Function for getting all contractor data */
  const {
    isLoading: FilterLoading,
    data: getFilterData,
    refetch,
  } = useGetAllPaginatedContractorsData(contractorData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton, sortColumn, sortOrder]);

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

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  /* Function to delete contractor */
  const deleteSiteConform = () => {
    getDeleteContractorById(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        {getFilterData?.is_available ? (
          <div>
            <div className={Styles.container}>
              <div className={Styles.box}>
                <div className={Styles.topHeading}>
                  <div className={Styles.heading}>
                    <div className={Styles.subHeading}>
                      <h3>CONTRACTOR</h3>
                    </div>
                    <div>
                      <Button
                        color="primary"
                        shape="rectangle"
                        justify="center"
                        size="small"
                        icon={<AddIcon color="white" />}
                        onClick={() => navigate('/contractor-add')}
                      >
                        Add Contractor
                      </Button>
                    </div>
                  </div>
                  <div className={Styles.searchBar}>
                    <Input
                      placeholder="Search Contractors"
                      width="300px"
                      prefixIcon={<SearchIcon />}
                      name="filter_value"
                      onChange={(e) => {
                        setFilterValues({
                          ...filterValues,
                          ['search_by_name']: e.target.value,
                        });
                        setCurrentPage(1)
                      }}
                    />
                  </div>
                </div>
                <div className={Styles.tableContainer}>
                  <div>
                    <table className={Styles.scrollable_table}>
                      <thead>
                        <tr>
                          <th>S No</th>
                          <th
                            onClick={() => handleSortByColumn('name', sortOrder, setSortOrder, setSortColumn)}
                          >
                            <div className={Styles.headingRow}>
                              <div>Name</div><div>
                                <FilterOrderIcon />
                              </div>
                            </div>
                          </th>
                          <th
                            onClick={() => handleSortByColumn('code', sortOrder, setSortOrder, setSortColumn)}
                          >
                            <div className={Styles.headingRow}>
                              <div>Code</div><div>
                                <FilterOrderIcon />
                              </div>
                            </div>
                          </th>
                          <th>Mobile Number</th>
                          <th>Description</th>
                          {activeButton === 'AC' && <th>Actions</th>}
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
                        {getFilterData?.content?.map((item: any, index: number) => (
                          <tr key={item.site_contractor_id}>
                            <td>{startingIndex + index}</td>
                            <td><span title={item.name}>
                              {item.name
                                ? item.name.substring(0, 50)
                                : '-'}
                            </span></td>
                            <td><span title={item.code}>
                              {item.code
                                ? item.code.substring(0, 50)
                                : '-'}
                            </span></td>
                            <td>
                              <span title={item.mobile_number}>
                                {item.mobile_number
                                  ? item.mobile_number.substring(0, 50)
                                  : '-'}
                              </span>
                            </td>
                            <td>
                              <span title={item.description}>
                                {item.description
                                  ? item.description.substring(0, 50)
                                  : '-'}
                              </span>
                            </td>
                            {activeButton === 'AC' && (
                              <td>
                                <div className={Styles.tableIcon}>
                                  <div>
                                    <EditIcon
                                      onClick={() =>
                                        navigate(
                                          `/contractor-edit/${item.site_contractor_id}`
                                        )
                                      }
                                    />
                                  </div>
                                </div>
                              </td>
                            )}
                          </tr>
                        ))}
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
              </div>
              <CustomDelete
                open={openDelete}
                title="Delete"
                contentLine1="Are you sure you want to delete this site? This action cannot be undone."
                contentLine2=""
                handleClose={handleCloseDelete}
                handleConfirm={deleteSiteConform}
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
        ) : (
          <div>
            <div className={Styles.subHeading}>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/_contractor_.jpg"
                  alt="contractor_img"
                  width="100%"
                  height="200px"
                  style={{ paddingTop: '35px', paddingBottom: '10px' }}
                />
              </div>
              <div>
                <h5>Contractor list is Empty</h5>
              </div>
              <div className={Styles.contentGap}>
                <span className={Styles.spanContent}>Go ahead, add new Contractors</span>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => navigate('/contractor-add')}
                >
                  Add Contractor
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
    </div>
  );
};

export default ContractorList;
