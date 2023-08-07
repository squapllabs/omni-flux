import React, { useState, useEffect } from 'react';
import Styles from '../../styles/userList.module.scss';
import EditIcon from '../menu/icons/editIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useGetAllClient, useDeleteClient, getByClient } from '../../hooks/client-hooks';
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
import Pagination from '../menu/pagination';
import SearchIcon from '../menu/icons/search';
import CustomSnackbar from '../ui/customSnackBar';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox'
import { IconButton } from '@mui/material';

const ClientList = () => {
  const { isLoading: getAllLoading } = useGetAllClient();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getByClient();
  console.log(getFilterData);
  
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
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [value,setValue] = useState();
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

  const handleClose = () => {
    setOpen(false);
  };

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const handleEdit = (value: any) => {
    setMode('EDIT');
    setClientID(value);
    setOpen(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };

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
          onSuccess: (data: { success: any; }, variables: any, context: any) => {
            if (data?.success) {
              setMessage('New Client has been successfully created');
              setOpenSnack(true);
              resetForm();
            }
            else {
              setMessage('Error occured in creating a new client');
              setOpenSnack(true);
            }
          },
        });
      }
    },
  });

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  const handleSearch = async () => {
    const demo: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(true);
  };

  const handleReset = async () => {
    const demo: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
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

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  return (
    <div>
      <div>
        <CustomLoader
          loading={isLoading === true ? getAllLoading : FilterLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>Add New Client</h3>
              <span className={Styles.content}>
                Manage your Client details here.
              </span>
            </div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.fields}>
                <div>
                  <Input
                    label="Name"
                    placeholder="Enter client name"
                    name="name"
                    value={formik.values.name}
                    onChange={formik.handleChange}
                    error={formik.touched.name && formik.errors.name}
                    width="100%"
                  />
                </div>
                <div>
                  <Input
                    label="Contact Detail"
                    placeholder="Enter client contact detail"
                    name="contact_details"
                    value={formik.values.contact_details}
                    onChange={formik.handleChange}
                    error={
                      formik.touched.contact_details && formik.errors.contact_details
                    }
                    width="100%"
                  />
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon />}
                  >
                    Add New Client
                  </Button>
                </div>
              </div>
            </form>
          </div>
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
                <table>
                  <thead>
                    <tr>
                      <th>S No</th>
                      <th>Client Name</th>
                      <th>Contact Details</th>
                      <th>Options</th>
                    </tr>
                  </thead>
                  <tbody>
                    {getFilterData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        <td></td>
                      </tr>
                    ) : (
                      ''
                    )}
                    {getFilterData?.content?.map((data: any, index: number) => (
                      <tr>
                        <td>{index + 1}</td>
                        <td>{data.name}</td>
                        <td>{data.contact_details}</td>
                        <td>

                          <EditIcon onClick={() => handleEdit(data.client_id)} />

                          {/* <IconButton
                            onClick={() =>deleteCategoryHandler(data.client_id)}
                          >
                            <DeleteIcon />
                          </IconButton> */}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
              <div className={Styles.pagination}>
                <Pagination
                  currentPage={currentPage}
                  totalPages={getFilterData?.total_page}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        </CustomLoader>
        <CustomEditDialog
          open={open}
          // handleClose={handleClose}
          // title="Edit Client"
          // subTitle="Please edit the client name"
          content={
            <ClientForm
              setOpen={setOpen}
              open={open}
              setReload={setReload}
              mode={mode}
              clientId={clientId}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
        <CustomDelete
          open={openDelete}
          title="Delete Client"
          contentLine1="Are you sure you want to delete this post? This action cannot be undone."
          contentLine2="Deleted Client will move to Inactive tab."
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
