import React, { useState } from 'react';
import Styles from '../../styles/userList.module.scss';
import MUIDataTable from 'mui-datatables';
import AddCircleOutlinedIcon from '@mui/icons-material/AddCircleOutlined';
import { Button } from '@mui/material';
import { Tooltip, IconButton } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import { useGetAllClient, useDeleteClient } from '../../hooks/client-hooks';
import ClientForm from './clientForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import CustomDialog from '../ui/customDialog';

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
  const handleAdd = (e: React.ChangeEvent<HTMLInputElement>) => {
    setMode('ADD');
    setOpen(true);
  };
  const handleEdit = (event: React.FormEvent, value: any) => {
    setMode('EDIT');
    setClientID(value);
    setOpen(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteClient = () => {
    getDeleteClientByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const columns = [
    {
      name: 'client_id',
      label: 'Uom',
      options: {
        display: false,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'name',
      label: 'Name',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'contact_details',
      label: 'Contact',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: '',
      label: 'Options',
      options: {
        sort: false,
        filter: false,
        searchable: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return (
            <div>
              <Tooltip title="Edit">
                <IconButton
                  aria-label="Edit"
                  size="small"
                  onClick={(e) => handleEdit(e, tableMeta.rowData[0])}
                >
                  <EditIcon />
                </IconButton>
              </Tooltip>
              <Tooltip title="Delete">
                <IconButton
                  aria-label="Delete"
                  size="small"
                  onClick={() => deleteClientHandler(tableMeta.rowData[0])}
                >
                  <DeleteIcon />
                </IconButton>
              </Tooltip>
            </div>
          );
        },
      },
    },
  ];

  const options = {
    filter: false,
    search: true,
    caseSensitive: false,
    print: false,
    download: false,
    viewColumns: false,
    selectableRows: 'none' as const,
    setTableProps: () => {
      return {
        size: 'small',
      };
    },
  };
  return (
    <div className={Styles.container}>
      <div className={Styles.buttonContainer}>
        <Button
          variant="contained"
          color="primary"
          startIcon={<AddCircleOutlinedIcon />}
          onClick={(e) => handleAdd(e)}
        >
          Add
        </Button>
      </div>
      <div className={Styles.tableContainer}>
        <MUIDataTable
          title={'Client List'}
          columns={columns}
          options={options}
          data={getAllClient}
        />
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
  );
};

export default ClientList;
