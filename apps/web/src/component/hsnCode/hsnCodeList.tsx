import React, { useState } from 'react';
import MUIDataTable from 'mui-datatables';
import Styles from '../../styles/gstList.module.scss';
import AddCircleOutlinedIcon from '@mui/icons-material/AddCircleOutlined';
import { useGetAllHsnCode, useDeleteHsnCode } from '../../hooks/hsnCode-hooks';
import { Button } from '@mui/material';
import { Tooltip, IconButton } from '@mui/material';
import CustomDialog from '../ui/customDialog';
import MySnackbar from '../ui/MySnackbar';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import HsnForm from './hsnCodeCreate';

const HscCodeList = () => {
  const { data: getAllHsnData, isLoading: loader } = useGetAllHsnCode();
  const { mutate: getDeleteHsnCodeByID } = useDeleteHsnCode();
  const [value, setValue] = useState(0);
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openPopup, setOpenPopup] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [hsnCodeId,setHsnCodeId] = useState();

  const deleteHsnCodeHandler = (id: number) => {
    setValue(id);
    setOpen(true);
  };

  const handleClose = () => {
    setOpen(false);
  };

  const deleteHscCode = () => {
    getDeleteHsnCodeByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const handleClosePopup = () => {
    setOpenPopup(false);
  };

  const handleAdd = (e: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    setMode('ADD');
    setOpenPopup(true);
  };
  const editHscCodeHandler = (value: any) => {
    setMode('EDIT');
    setHsnCodeId(value);
    setOpenPopup(true);
  };


  const columns = [
    {
      name: 'hsn_code_id',
      label: 'Hsn',
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
      name: 'code',
      label: 'Code',
      options: {
        display: true,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'description',
      label: 'Description',
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
                  onClick={() => editHscCodeHandler(tableMeta.rowData[0])}
                >
                  <EditIcon />
                </IconButton>
              </Tooltip>
              <Tooltip title="Delete">
                <IconButton
                  aria-label="Delete"
                  size="small"
                  onClick={() => deleteHsnCodeHandler(tableMeta.rowData[0])}
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
        onClick={(e) => handleAdd(e)}
      >
        Add
      </Button>
    </div>
    <div className={Styles.tableContainer}>
      <MUIDataTable
        title={`Hsn Code List (${
          getAllHsnData?.length ? getAllHsnData?.length : 0
        })`}
        data={getAllHsnData}
        columns={columns}
        options={options}
      />
    </div>
    <CustomDialog
      open={open}
      handleClose={handleClose}
      title="Delete Hsc Code"
      content="Are you want to delete this Hsc Code?"
      handleConfirm={deleteHscCode}
    />
    <CustomDialogBox
        open={openPopup}
        handleClose={handleClosePopup}
        title="Gst Creation"
        content={
          <HsnForm
            setOpenPopup={setOpenPopup}
            open={openPopup}
            setReload={setReload}
            mode={mode}
            hsnCodeId={hsnCodeId}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
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

export default HscCodeList;
