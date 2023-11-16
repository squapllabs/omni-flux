import React, { useState, useEffect } from 'react';
import Styles from '../../styles/projectWorkBreakDownList.module.scss';
import SearchIcon from '../menu/icons/search';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Pagination from '../menu/pagination';
import CustomLoader from '../ui/customLoader';
import { useGetBySearchSiteData, useDeleteSite } from '../../hooks/site-hooks';
import AddIcon from '../menu/icons/addIcon';
import { useNavigate } from 'react-router';
import EditIcon from '../menu/icons/editIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';

const ProjectWorkBreakList = () => {
  const { mutate: getDeleteSiteById } = useDeleteSite();
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(3); // Set initial value to 1
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = useGetBySearchSiteData();
  const navigate = useNavigate();

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
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
      type: 'Site',
    };
    await postDataForFilter(demo);
    setTotalPages(getFilterData?.total_page);
    setIsLoading(false);
    setFilter(true);
  };
  const handleReset = async () => {
    setFilterValues({
      search_by_name: '',
    });
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

  const deleteSite = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const deleteSiteConform = () => {
    getDeleteSiteById(value);
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
        <div className={Styles.container}>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Project Site</h3>
              <span className={Styles.content}>
                Manage your project site across your application
              </span>
            </div>
            <div className={Styles.searchField}>
              <div className={Styles.inputFilter}>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="search_by_name"
                  value={filterValues.search_by_name}
                  onChange={(e) => handleFilterChange(e)}
                  placeholder="Search by name"
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
                <Button
                  className={Styles.resetButton}
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon />}
                  onClick={() => navigate('/site-add')}
                >
                  Add
                </Button>
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.tableContainer}>
              <div>
                <table>
                  <thead>
                    <tr>
                      <th>S No</th>
                      <th>Name</th>
                      <th>Code</th>
                      <th>Mobile Number</th>
                      <th>Description</th>
                      {activeButton === 'AC' && <th></th>}
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
                        <td>{item.name}</td>
                        <td>{item.code}</td>
                        <td>{item.mobile_number}</td>
                        {/* <td>{item.description}</td> */}
                        <td>
                          <span title={item.description}>
                            {item.description
                              ? item.description.substring(0, 30)
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
                                      `/site-edit/${item.site_contractor_id}`
                                    )
                                  }
                                />
                              </div>
                              <div>
                                <DeleteIcon
                                  onClick={() =>
                                    deleteSite(item.site_contractor_id)
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
          <CustomDelete
            open={openDelete}
            title="Delete"
            contentLine1="Are you sure you want to delete this post? This action cannot be undone."
            // contentLine2="Deleted site will move to Inactive tab."
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
      </CustomLoader>
    </div>
  );
};

export default ProjectWorkBreakList;
