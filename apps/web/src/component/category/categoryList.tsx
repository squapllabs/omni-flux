import React, { useState } from 'react';
import Styles from '../../styles/categoryList.module.scss';
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
import Input from '../../component/ui/Input';
import { useFormik} from 'formik';
import { getCreateValidateyup } from '../../helper/constants/category/category-constants';
import { createCategory } from '../../hooks/category-hooks';
import * as Yup from 'yup';
import Select from '../ui/Select';
import { useGetAllProject } from '../../hooks/project-hooks';

/**
 * Function for  CategoryList
 */
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
  const validationSchema = getCreateValidateyup(Yup);
  const [initialValues, setInitialValues] = useState({
    category_id: '',
    name: '',
    budget: '',
    project_id: '',
  });
  const { mutate: createNewCategory } = createCategory();
  const { data: getAllProjectList = [] } = useGetAllProject();
  const [selectedValue, setSelectedValue] = useState('');

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
  /**
   * Function for editing the Category
   */
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
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          project_id: Number(selectedValue),
        };
        createNewCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              resetForm();
              setMessage('Category created');
              setOpenSnack(true);
            }
          },
        });
      }
    },
  });

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
      <div className={Styles.textContent}>
        <h3>Add New Categories</h3>
        <span className={Styles.content}>
          Manage your raw materials (Raw, Semi Furnished & Finished).
        </span>
      </div>

      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.fields}>
          <div className={Styles.projectField}>
            <span className={Styles.projectHeading}>Project</span>
            <Select
              options={getAllProjectList}
              onChange={handleDropdownChange}
              value={selectedValue}
              defaultLabel="Select from options"
              width="100%"
            />
            {formik.touched.project_id && formik.errors.project_id && (
              <div className={Styles.error}>{formik.errors.project_id}</div>
            )}
          </div>
          <div>
            <Input
              name="name"
              label="Category Name"
              placeholder="Enter category name"
              value={formik.values.name}
              onChange={formik.handleChange}
              error={formik.touched.name && formik.errors.name}
            />
          </div>
          <div>
            <Input
              name="budget"
              label="Budget"
              placeholder="Enter budget"
              value={formik.values.budget}
              onChange={formik.handleChange}
              error={formik.touched.budget && formik.errors.budget}
            />
          </div>
          <div>
            <Button
              text="Add New Category"
              backgroundColor="#7F56D9"
              fontSize={14}
              fontWeight={500}
              width={135}
            />
          </div>
        </div>
      </form>
      <div className={Styles.textContent}>
        <h3>List of Categories</h3>
        <span className={Styles.content}>
          Manage your raw materials (Raw, Semi Furnished & Finished).
        </span>
      </div>
      <div className={Styles.searchField}>
        <div>
          <Input name="budget" placeholder="Search by item name" width="160%" />
        </div>
        <div className={Styles.searchButton}>
          <Button
            text="Search"
            fontSize={14}
            fontWeight={500}
            width={120}
            backgroundColor="#F6F4EB"
            textColor="#7F56D9"
          />
          <Button
            text="Reset"
            fontSize={14}
            fontWeight={500}
            width={120}
            backgroundColor="white"
            textColor="#B2B2B2"
          />
        </div>
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
