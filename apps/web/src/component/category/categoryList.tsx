import React, { useState } from 'react';
import Styles from '../../styles/userList.module.scss';
import MUIDataTable from 'mui-datatables';
import { Tooltip, IconButton } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import {
  useGetAllCategory,
  useDeleteCategory,
} from '../../hooks/category-hooks';
import CategoryForm from './categoryForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import CustomDialog from '../ui/customDialog';
import Button from '../menu/button';

const CategoryList = () => {
  const { data: getAllCategory } = useGetAllCategory();
  const { mutate: getDeleteCategoryByID } = useDeleteCategory();
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [categoryId, setCategoryID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const deleteCategoryHandler = (id: any) => {
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
    setCategoryID(value);
    setOpen(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteCategory = () => {
    getDeleteCategoryByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const columns = [
    {
      name: 'category_id',
      label: 'category',
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
      name: 'budget',
      label: 'Budget',
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
                  onClick={() => deleteCategoryHandler(tableMeta.rowData[0])}
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
          title={'Category List'}
          columns={columns}
          options={options}
          data={getAllCategory}
        />
      </div>
      <CustomDialogBox
        open={open}
        handleClose={handleClose}
        title="Category Form"
        content={
          <CategoryForm
            setOpen={setOpen}
            open={open}
            setReload={setReload}
            mode={mode}
            categoryId={categoryId}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
      />
      <CustomDialog
        open={openDelete}
        handleClose={handleCloseDelete}
        title="Delete Category"
        content="Are you want to delete this Category?"
        handleConfirm={deleteCategory}
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

export default CategoryList;
