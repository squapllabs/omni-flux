import React, { useState } from 'react';
import Styles from '../../styles/userList.module.scss';
import MUIDataTable from 'mui-datatables';
import { Tooltip, IconButton } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import {
  useGetAllSubSubcategory,
  useDeleteSubSubcategory,
} from '../../hooks/subSubCategory-hooks';
import CustomDialog from '../ui/customDialog';
import SubSubForm from './subSubForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import Button from '../menu/button';

const SubSubCategoryList = () => {
  const { data: getAllSubSubCategory ,isLoading: loader } = useGetAllSubSubcategory();
  const { mutate: getDeleteSubSubCategoryByID, } = useDeleteSubSubcategory();
  const [value, setValue] = useState(0);
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openPopup, setOpenPopup] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [subSubCategoryId, setSubSubCategoryId] = useState();

  const deleteSubSubCategoryHandler = (id: number) => {
    setValue(id);
    setOpen(true);
  };

  const handleClose = () => {
    setOpen(false);
  };

  const deleteSubSubCategory = () => {
    getDeleteSubSubCategoryByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleAdd = (e: React.MouseEvent<HTMLButtonElement, MouseEvent>) => {
    setMode('ADD');
    setOpenPopup(true);
  };

  const handleEdit = (event: React.FormEvent, value: any) => {
    setMode('EDIT');
    setSubSubCategoryId(value);
    setOpenPopup(true);
  };

  const handleClosePopup = () => {
    setOpenPopup(false);
  };
  const columns = [
    {
      name: 'sub_sub_category_details',
      label: 'sub_sub_category',
      options: {
        display: false,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return (
            <div>
              <span>{value.sub_sub_category_id}</span>
            </div>
          );
        },
      },
    },
    {
      name: 'sub_sub_category_details',
      label: 'Name',
      options: {
        display: true,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return (
            <div>
              <span>{value.sub_sub_category_name}</span>
            </div>
          );
        },
      },
    },
    {
      name: 'sub_category_details',
      label: 'Sub Category',
      options: {
        display: true,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return (
            <div>
              <span>{value.sub_category_name}</span>
            </div>
          );
        },
      },
    },
    {
      name: 'sub_sub_category_details',
      label: 'Budget',
      options: {
        display: true,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return (
            <div>
              <span>{value.budget}</span>
            </div>
          );
        },
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
                  onClick={() =>
                    deleteSubSubCategoryHandler(tableMeta.rowData[0])
                  }
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
          text="Add"
          backgroundColor="#7F56D9"
          fontSize={14}
          fontWeight={500}
          width={100}
          onClick={(e) => handleAdd(e)}
        />
      </div>
      <div className={Styles.tableContainer}>
        <MUIDataTable
          title={'Sub Sub Categories List'}
          columns={columns}
          options={options}
          data={getAllSubSubCategory}
        />
      </div>
      <CustomDialog
        open={open}
        handleClose={handleClose}
        title="Delete Sub Sub Category"
        content="Are you want to delete this category?"
        handleConfirm={deleteSubSubCategory}
      />
      <MySnackbar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        severity={'success'}
        autoHideDuration={1000}
      />
      <CustomDialogBox
        open={openPopup}
        handleClose={handleClosePopup}
        title="Sub Sub Category Creation"
        content={
          <SubSubForm
            setOpenPopup={setOpenPopup}
            open={openPopup}
            setReload={setReload}
            mode={mode}
            subSubCategoryId={subSubCategoryId}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
      />
    </div>
  );
};

export default SubSubCategoryList;
