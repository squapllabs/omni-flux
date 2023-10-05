import React, { useState, useEffect } from 'react';
import Styles from '../../styles/userList.module.scss';
import EditIcon from '../menu/icons/newEditIcon';
import DeleteIcon from '../menu/icons/newDeleteIcon';
import {
  useDeleteClient,
  getByClient,
  useGetAllPaginatedClient,
} from '../../hooks/client-hooks';
import ClientForm from './clientForm';
import CustomEditDialog from '../ui/customEditDialogBox';
import Button from '../ui/Button';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import { createClient } from '../../hooks/client-hooks';
import * as Yup from 'yup';
import { getClientValidateyup } from '../../helper/constants/client-constants';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/CustomPagination';
import SearchIcon from '../menu/icons/search';
import CustomSnackbar from '../ui/customSnackBar';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSidePopup from '../ui/CustomSidePopup';

/* Function for Client List */
const ClientList = () => {
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getByClient();

  const { mutate: getDeleteClientByID } = useDeleteClient();
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [clientId, setClientID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const validationSchema = getClientValidateyup(Yup);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [value, setValue] = useState();
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const [openClientForm, setOpenClientForm] = useState(false);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const { mutate: createNewClient } = createClient();
  const [initialValues, setInitialValues] = useState({
    name: '',
    contact_details: '',
    client_id: '',
  });
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');

  const clientData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    global_search: filterValues.search_by_name,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedClient(clientData);

  const handleClose = () => {
    setOpen(false);
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

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
  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  /* Function for deleting a Client from the list */
  const deleteClient = () => {
    getDeleteClientByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          name: values.name,
          contact_details: values.contact_details,
        };
        createNewClient(Object, {
          onSuccess: (data: { success: any }, variables: any, context: any) => {
            if (data?.success) {
              setMessage('Client created');
              setOpenSnack(true);
              resetForm();
            } else {
              setMessage('Error occured in creating a new client');
              setOpenSnack(true);
            }
          },
        });
      }
    },
  });

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

  /* Function for Searching a client data from the list */
  const handleSearch = async () => {
    const clientData: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(clientData);
    setDataShow(true);
    setIsLoading(false);
    setFilter(true);
  };
  /*Function for reseting the list to actual state after search*/
  const handleReset = async () => {
    setDataShow(false);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
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

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  const handleClientFormClose = () => {
    setOpenClientForm(false);
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
          <div className={Styles.topheader}>
            <div className={Styles.title}>
              <div>
                <h3>Add New Client</h3>
              </div>
              <div>
                <span className={Styles.description}>
                  Manage your Client details here.
                </span>
              </div>
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
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Clients</h3>
              <span className={Styles.content}>
                Manage your Client Details here.
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
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
            </div>
            <div className={Styles.tableContainer}>
              <div>
                <table className={Styles.scrollable_table}>
                  <thead>
                    <tr>
                      <th>#</th>
                      <th>Client Name</th>
                      <th>Contact Details</th>
                      {activeButton === 'AC' && <th>Actions</th>}
                    </tr>
                  </thead>
                  <tbody>
                    {dataShow ? (
                      getFilterData?.total_count === 0 ? (
                        <tr>
                          <td></td>
                          <td></td>
                          <td>No data found</td>
                          {activeButton === 'AC' && <td></td>}
                        </tr>
                      ) : (
                        getFilterData?.content?.map(
                          (data: any, index: number) => (
                            <tr key={data.client_id}>
                              <td>{startingIndex + index}</td>
                              <td>{data.name}</td>
                              <td>{data.contact_details}</td>
                              {activeButton === 'AC' && (
                                <td>
                                  <div className={Styles.tablerow}>
                                    <EditIcon
                                      onClick={() => handleEdit(data.client_id)}
                                    />
                                    <DeleteIcon
                                      onClick={() =>
                                        deleteCategoryHandler(data.client_id)
                                      }
                                    />
                                  </div>
                                </td>
                              )}
                            </tr>
                          )
                        )
                      )
                    ) : initialData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        {activeButton === 'AC' && <td></td>}
                      </tr>
                    ) : (
                      initialData?.content?.map((data: any, index: number) => (
                        <tr key={data.client_id}>
                          {/* <td>{index + 1}</td> */}
                          <td>{startingIndex + index}</td>
                          <td>{data.name}</td>
                          <td>{data.contact_details}</td>
                          {activeButton === 'AC' && (
                            <td>
                              <div className={Styles.tablerow}>
                                <EditIcon
                                  onClick={() => handleEdit(data.client_id)}
                                />
                                <DeleteIcon
                                  onClick={() =>
                                    deleteCategoryHandler(data.client_id)
                                  }
                                />
                              </div>
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
