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
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
import {
  useDeleteMachinery,
  useGetAllPaginatedMachinery,
} from '../../hooks/machinery-hooks';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import { handleSortByColumn } from './../../helper/common-function'

const MachineryList = () => {

  const { mutate: getDeleteMachineryByID } = useDeleteMachinery();
  const navigate = useNavigate();
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');
  const [sortColumn, setSortColumn] = useState();
  const [sortOrder, setSortOrder] = useState('desc');

  const machineryData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: sortColumn === '' ? 'created_date' : sortColumn,
    order_by_direction: sortOrder,
    global_search: filterValues.search_by_name,
    status: activeButton,
  };

  const {
    data: getFilterData,
    isLoading: FilterLoading,
    refetch,
  } = useGetAllPaginatedMachinery(machineryData);

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
                </div>
              </div>
              <div className={Styles.tableContainer}>
                <div>
                  <table className={Styles.scrollable_table}>
                    <thead>
                      <tr>
                        <th>#</th>
                        <th
                          onClick={() => handleSortByColumn('machinery_name', sortOrder, setSortOrder, setSortColumn)}
                        >
                          <div className={Styles.headingRow}>
                            <div>Machinery Name</div><div>
                              <FilterOrderIcon />
                            </div>
                          </div>
                        </th>
                        <th
                          onClick={() => handleSortByColumn('rate', sortOrder, setSortOrder, setSortColumn)}
                        >
                          <div className={Styles.headingRow}>
                            <div>Rate</div><div>
                              <FilterOrderIcon />
                            </div>
                          </div>
                        </th>
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
                          <td>
                            <span title={data.machinery_name}>
                              {data.machinery_name
                                ? data.machinery_name.substring(0, 50)
                                : '-'}
                            </span>
                          </td>
                          <td>{formatBudgetValue(data.rate || '-')}</td>
                          <td>{data.uom_data.name}</td>
                          <td><span title={data.operational_status}>
                            {data.operational_status
                              ? data.operational_status.substring(0, 50)
                              : '-'}
                          </span></td>
                          {activeButton === 'AC' && (
                            <td>
                              <div className={Styles.tablerow}>
                                <EditIcon
                                  onClick={() =>
                                    navigate(`/edit-machinery/${data.machinery_id}`)
                                  }
                                />
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
                    height="200px"
                  />
                </div>
                <div>
                  <h5>Machineries list is Empty</h5>
                </div>
                <div className={Styles.contentGap}>
                  <span className={Styles.spanContent}>Go ahead, add new Machineries</span>
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
