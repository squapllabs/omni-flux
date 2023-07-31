import React, { useState } from 'react';
import Styles from '../../styles/userList.module.scss';
import { IconButton } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import { useGetAllClient, useDeleteClient } from '../../hooks/client-hooks';
import ClientForm from './clientForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import CustomDialog from '../ui/customDialog';
import Button from '../ui/Button';
import Input from '../ui/Input';
import { useFormik } from 'formik';
import { createClient } from '../../hooks/client-hooks';


const ClientList = () => {
  const { data: getAllClient } = useGetAllClient();
  const { mutate: getDeleteClientByID } = useDeleteClient();
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [clientId, setClientID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const deleteClientHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  const handleClose = () => {
    setOpen(false);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const handleEdit = (event: React.FormEvent, value: any) => {
    setMode('EDIT');
    setClientID(value);
    setOpen(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteClient = (event: React.FormEvent, value: any) => {
    getDeleteClientByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };
  const { mutate: createNewClient } = createClient();
  const [initialValues, setInitialValues] = useState({
    name: '',
    contact_details: '',
    client_id: '',
  });
  const formik = useFormik({
    initialValues,
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
              setMessage('Client created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });


  return (
    <div>
      <div>
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
          <div className={Styles.tableContainer}>
            <div>
              <table>
                <thead>
                  <tr>
                    <th>Client Name</th>
                    <th>Contact Details</th>
                    <th>Options</th>
                  </tr>
                </thead>
                <tbody>
                  {getAllClient?.map((data: any) => (
                    <tr>
                      <td>{data.name}</td>
                      <td>{data.contact_details}</td>
                      <td>
                        <IconButton
                          onClick={(e) => handleEdit(e, data.client_id)}
                        >
                          <EditIcon />
                        </IconButton>
                        <IconButton
                          onClick={(e) =>
                            deleteClient(e, data.client_id)
                          }
                        >
                          <DeleteIcon />
                        </IconButton>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
        <CustomDialogBox
          open={open}
          handleClose={handleClose}
          title="Client Form"
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
        <CustomDialog
          open={openDelete}
          handleClose={handleCloseDelete}
          title="Delete Client"
          content="Are you want to delete this Client?"
          handleConfirm={deleteClient}
        />
        <MySnackbar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          severity={'success'}
          autoHideDuration={1000}
        />
      </div>
    </div>
  );
};

export default ClientList;
