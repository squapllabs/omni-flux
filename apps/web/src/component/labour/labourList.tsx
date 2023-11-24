import React, { useState, useEffect } from 'react';
import Styles from '../../styles/labourList.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import EditIcon from '../menu/icons/newEditIcon';
import SearchIcon from '../menu/icons/search';
import CustomDelete from '../ui/customDeleteDialogBox';
import { useDeleteLabour, useGetAllLabour } from '../../hooks/labour-hooks';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/CustomPagination';
import { formatBudgetValue } from '../../helper/common-function';
import CustomPopup from '../ui/CustomSidePopup';
import CustomLabourAddPopup from './labourAdd';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import { handleSortByColumn } from './../../helper/common-function';

/* Function to list labour */
const LabourList = () => {
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [filterValues, setFilterValues] = useState({
    global_search: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [mode, setMode] = useState('');
  const [value, setValue] = useState();
  const [labourId, setLabourId] = useState('');
  const [message, setMessage] = useState('');
  const [sortColumn, setSortColumn] = useState('');
  const [sortOrder, setSortOrder] = useState('desc');
  const [modalOpen, setModalOpen] = useState(false);

  const object: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: sortColumn === '' ? 'created_date' : sortColumn,
    order_by_direction: sortOrder,
    status: activeButton,
    global_search: filterValues.global_search,
  };
  const {
    isLoading: getAllLoadingLabourData,
    data: initialData,
    refetch,
  } = useGetAllLabour(object);

  const { mutate: getDeleteLabourByID } = useDeleteLabour();

  const handleAddLabourData = () => {
    setOpen(true);
    setMode('ADD');
    setModalOpen(true);
  };

  const handleEdit = (value: any) => {
    setLabourId(value);
    setOpen(true);
    setMode('EDIT');
    setModalOpen(true);
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

  /* Function for changing the table page */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleClosePopup = () => {
    setOpen(false);
    setModalOpen(false)
  };

  /* Function for changing no of rows in pagination */
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
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
  };

  useEffect(() => {
    if (modalOpen === true) {
      document.body.style.overflow = 'hidden';
    } else {
      document.body.style.overflow = 'auto';
    }
  }, [modalOpen]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.container}>
      <CustomLoader loading={getAllLoadingLabourData} size={48} color="#333C44">
        {initialData?.is_available ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.subHeading}>
                  <h3>LABOURS</h3>
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() => handleAddLabourData()}
                  >
                    Add Labour
                  </Button>
                </div>
              </div>
              <div className={Styles.filters}>
                <div className={Styles.searchFeild}>
                  <Input
                    placeholder="Search Labours"
                    width="300px"
                    prefixIcon={<SearchIcon />}
                    name="filter_value"
                    onChange={(e) => {
                      setFilterValues({
                        ...filterValues,
                        global_search: e.target.value,
                      });
                      setCurrentPage(1);
                    }}
                  />
                </div>
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
                          onClick={() =>
                            handleSortByColumn(
                              'labour_type',
                              sortOrder,
                              setSortOrder,
                              setSortColumn
                            )
                          }
                        >
                          <div className={Styles.headingRow}>
                            <div>Labour Type</div>
                            <div>
                              <FilterOrderIcon />
                            </div>
                          </div>
                        </th>
                        <th>UOM Type</th>
                        <th
                          onClick={() =>
                            handleSortByColumn(
                              'rate',
                              sortOrder,
                              setSortOrder,
                              setSortColumn
                            )
                          }
                        >
                          <div className={Styles.headingRow}>
                            <div>Rate</div>
                            <div>
                              <FilterOrderIcon />
                            </div>
                          </div>
                        </th>
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
                        initialData?.content?.map(
                          (item: any, index: number) => (
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
                                        onClick={() =>
                                          handleEdit(item?.labour_id)
                                        }
                                      />
                                    </div>
                                  </div>
                                </td>
                              )}
                            </tr>
                          )
                        )
                      )}
                    </tbody>
                  </table>
                </div>
                <div className={Styles.pagination}>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={initialData?.total_page}
                    totalCount={initialData?.total_page}
                    rowsPerPage={rowsPerPage}
                    onPageChange={handlePageChange}
                    onRowsPerPageChange={handleRowsPerPageChange}
                  />
                </div>
              </div>
            </div>
          </div>
        ) : (
          <div>
            <div className={Styles.subHeading}></div>
            <div className={Styles.emptyDataHandling}>
              <div>
                <img
                  src="/labours_img.jpg"
                  alt="aa"
                  width="100%"
                  height="200px"
                />
              </div>
              <div>
                <h5>The Labours list is empty</h5>
              </div>
              <div className={Styles.contentGap}>
                <span className={Styles.spanContent}>
                  Go ahead, add new labour list
                </span>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => handleAddLabourData()}
                >
                  Add Labour
                </Button>
              </div>
            </div>
          </div>
        )}
      </CustomLoader>
      <CustomDelete
        open={openDelete}
        title="Delete Labour"
        contentLine1="Are you sure you want to delete this Labour ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteLabour}
      />
      <CustomPopup
        title={mode === 'ADD' ? 'NEW LABOUR' : 'EDIT LABOUR'}
        open={open}
        handleClose={handleClosePopup}
        content={
          <CustomLabourAddPopup
            setOpen={setOpen}
            setModalOpen={setModalOpen}
            open={open}
            mode={mode}
            labourId={labourId}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
      />
    </div>
  );
};

export default LabourList;
