import React, { useState, useEffect } from 'react';
import Styles from '../../styles/clientList.module.scss';
import EditIcon from '../menu/icons/newEditIcon';
import {
  useDeleteClient,
  useGetAllPaginatedClient,
} from '../../hooks/client-hooks';
import ClientForm from './clientForm';
import Button from '../ui/Button';
import Input from '../ui/Input';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/CustomPagination';
import SearchIcon from '../menu/icons/search';
import CustomSnackbar from '../ui/customSnackBar';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSidePopup from '../ui/CustomSidePopup';
import FilterOrderIcon from '../menu/icons/filterOrderIcon';
import { handleSortByColumn } from './../../helper/common-function';

/* Function for Client List */
const ClientList = () => {

  const { mutate: getDeleteClientByID } = useDeleteClient();
  const [openDelete, setOpenDelete] = useState(false);
  const [clientId, setClientID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [value, setValue] = useState();
  const [openClientForm, setOpenClientForm] = useState(false);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [sortColumn, setSortColumn] = useState('');
  const [sortOrder, setSortOrder] = useState('desc');

  const clientData = {
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
  } = useGetAllPaginatedClient(clientData);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton, sortColumn, sortOrder]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 1000);
    return () => clearTimeout(handleSearch);
  }, [filterValues]);

  /* Function for Closing the delete popup */
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  /* Function for editing a client data in the list */
  const handleEdit = (value: any) => {
    setMode('EDIT');
    setClientID(value);
    setOpenClientForm(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  /* Function for deleting a Client from the list */
  const deleteClient = () => {
    getDeleteClientByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  /* Funtion to change page */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  /* Function to close the client create and edit form */
  const handleClientFormClose = () => {
    setOpenClientForm(false);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div>
      <div>
        <CustomLoader
          loading={getAllLoadingPaginated}
          size={48}
          color="#333C44"
        >
          {initialData?.is_available ? (
            <div>
              <div className={Styles.topHeading}>
                <div className={Styles.heading}>
                  <div className={Styles.subHeading}>
                    <h3>CLIENT</h3>
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
                        setOpenClientForm(true);
                      }}
                    >
                      Add Client
                    </Button>
                  </div>
                </div>
                <div className={Styles.searchBar}>
                  <div className={Styles.searchFeild}>
                    <Input
                      placeholder="Search Client"
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
                                'name',
                                sortOrder,
                                setSortOrder,
                                setSortColumn
                              )
                            }
                          >
                            <div className={Styles.headingRow}>
                              <div>Client Name</div>
                              <div>
                                <FilterOrderIcon />
                              </div>
                            </div>
                          </th>
                          <th>Contact Details</th>
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
                            (data: any, index: number) => (
                              <tr key={data.client_id}>
                                <td>{startingIndex + index}</td>
                                <td>{data.name}</td>
                                <td>{data.contact_details}</td>
                                {activeButton === 'AC' && (
                                  <td>
                                    <div className={Styles.tablerow}>
                                      <EditIcon
                                        onClick={() =>
                                          handleEdit(data.client_id)
                                        }
                                      />
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
                </div>
                <div className={Styles.pagination}>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={initialData?.total_page}
                    totalCount={initialData?.total_count}
                    rowsPerPage={rowsPerPage}
                    onPageChange={handlePageChange}
                    onRowsPerPageChange={handleRowsPerPageChange}
                  />
                </div>
              </div>
            </div>
          ) : (
            <div>
              <div className={Styles.subHeading}></div>
              <div className={Styles.emptyDataHandling}>
                <div>
                  <img src="/client.jpg" alt="aa" width="100%" height="200px" />
                </div>
                <div>
                  <h5>Client list is Empty</h5>
                </div>
                <div className={Styles.contentGap}>
                  <span className={Styles.spanContent}>
                    Go ahead, add new Clients
                  </span>
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
                      setOpenClientForm(true);
                    }}
                  >
                    Add Client
                  </Button>
                </div>
              </div>
            </div>
          )}
        </CustomLoader>
        <CustomSidePopup
          open={openClientForm}
          title={mode === 'EDIT' ? 'Edit Client' : 'Add Client'}
          handleClose={handleClientFormClose}
          content={
            <ClientForm
              open={openClientForm}
              setOpen={setOpenClientForm}
              reload={reload}
              setReload={setReload}
              openSnack={openSnack}
              setOpenSnack={setOpenSnack}
              message={message}
              setMessage={setMessage}
              mode={mode}
              clientId={clientId}
            />
          }
        />
        <CustomDelete
          open={openDelete}
          title="Delete Client"
          contentLine1="Are you sure you want to delete this Client ?"
          contentLine2=""
          handleClose={handleCloseDelete}
          handleConfirm={deleteClient}
        />
        <CustomSnackbar
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

export default ClientList;
