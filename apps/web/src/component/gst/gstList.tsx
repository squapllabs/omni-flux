import React, { useState } from 'react';
import Styles from '../../styles/gstList.module.scss';
import MUIDataTable from 'mui-datatables';
import AddCircleOutlinedIcon from '@mui/icons-material/AddCircleOutlined';
import {useGetAllGst, useDeleteGst } from '../../hooks/gst-hooks';
import { useNavigate } from 'react-router';
import { Button } from '@mui/material';
import { Tooltip, IconButton} from '@mui/material';
import CustomDialog from '../ui/customDialog';
import MySnackbar from '../ui/MySnackbar';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';

const GstList = () => {
  const { data: getAllGstData, isLoading: loader } = useGetAllGst();
  
  const { mutate: getDeleteGstByID } = useDeleteGst();
  const [open, setOpen] = useState(false);
  const [openDeleteSnack, setOpenDeleteSnack] = useState(false);
  const [value, setValue] = useState(0);
  const [message, setMessage] = useState('');
  const navigate = useNavigate();

  const deleteUserHandler = (id: any) => {
    setValue(id);
    setOpen(true);
  };
  const handleClose = () => {
    setOpen(false);
  };
  const handleSnackBarClose = () => {
    setOpenDeleteSnack(false);
  };

  const deleteUser = () => {
    getDeleteGstByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenDeleteSnack(true);
  };

 

  const columns = [
    {
      name: 'gst_id',
      label: 'gst',
      options: {
        display: false,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'S No',
      label: 'S No',
      options: {
        display: true,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return tableMeta.rowIndex + 1;
        },
      },
    },

    {
      name: 'rate',
      label: 'Rate',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'cgst_rate',
      label: 'Cgst Rate',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'igst_rate',
      label: 'Igst Rate',
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
                  onClick={() => navigate(`/gst-edit/${tableMeta.rowData[0]}`)}
                >
                  <EditIcon />
                </IconButton>
              </Tooltip>
              <Tooltip title="Delete">
                <IconButton
                  aria-label="Delete"
                  size="small"
                  onClick={() => deleteUserHandler(tableMeta.rowData[0])}
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
    textLabels: {
      body: {
        noMatch: loader ? 'Loading...' : 'Sorry , No Records found',
      },
    },
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
          onClick={() => navigate('/gst-create')}
        >
          Add
        </Button>
      </div>
      <div className={Styles.tableContainer}>
            <MUIDataTable
              title={`Gst List (${
                getAllGstData?.length ? getAllGstData?.length : 0
              })`}
              data={getAllGstData}
              columns={columns}
              options={options}
            />
      </div>
      <CustomDialog
        open={open}
        handleClose={handleClose}
        title="Delete Gst"
        content="Are you want to delete this gst?"
        handleConfirm={deleteUser}
      />
      <MySnackbar
        open={openDeleteSnack}
        message={message}
        onClose={handleSnackBarClose}
        severity={'success'}
        autoHideDuration={1000}
      />
    </div>
  );
};

export default GstList;
