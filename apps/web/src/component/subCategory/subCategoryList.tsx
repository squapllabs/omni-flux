import React, { useState } from 'react';
import Styles from '../../styles/subCategoryList.module.scss';
import MUIDataTable from 'mui-datatables';
import {
  Tooltip,
  IconButton,
  MenuItem,
  InputLabel,
  Select,
} from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import {
  useGetAllSubcategory,
  useDeleteSubcategory,
} from '../../hooks/subCategory-hooks';
import CategoryForm from './SubCategoryForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import CustomDialog from '../ui/customDialog';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import { useFormik } from 'formik';
import { getCreateValidateyup } from '../../helper/constants/category/subcategory-constants';
import * as Yup from 'yup';
import {
  createSubcategory,
  updateSubcategory,
} from '../../hooks/subCategory-hooks';
import { useGetAllCategory } from '../../hooks/category-hooks';
import SearchIcon from '../menu/icons/search';
import SubCategoryService from '../../service/subCategory-service';
const SubCategoryList = () => {
  const validationSchema = getCreateValidateyup(Yup);
  const { data: getAllSubCategory } = useGetAllSubcategory();
  const { data: getAllCategory } = useGetAllCategory();
  const { mutate: getDeleteSubcategoryByID } = useDeleteSubcategory();
  const { mutate: createNewSubcategory } = createSubcategory();
  const [initialValues, setInitialValues] = useState({
    sub_category_id: '',
    name: '',
    budget: '',
    category_id: '',
  });
  const [filterValues, setFilterValues] = useState({});
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        name: values.name,
        budget: Number(values.budget),
        category_id: values.category_id,
      };
      createNewSubcategory(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success) {
            setMessage('Sub category is created successfully');
            setOpenSnack(true);
            resetForm();
          }
        },
      });
    },
  });

  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [openDelete, setOpenDelete] = useState(false);
  const [open, setOpen] = useState(false);
  const [subCategoryId, setSubcategoryID] = useState();
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
    setSubcategoryID(value);
    setOpen(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteSubcategory = () => {
    getDeleteSubcategoryByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };
  const handleSearch = async () => {
    let demo: any = {
      offset: 0,
      limit: 3,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'IN',
      ...filterValues,
    };
    console.log('data', demo);

    const data = await SubCategoryService.filterSubCategory(demo);
    console.log('data', data);
  };
  const handleFilterChange = (event) => {
    setFilterValues({
      ...filterValues,
      [event.target.name]: event.target.value,
    });
  };
  const columns = [
    {
      name: 'sub_category_id',
      label: 'category',
      options: {
        display: false,
        filter: false,
        sort: false,
      },
    },
    {
      name: 'category',
      label: 'Category',
      options: {
        display: true,
        filter: false,
        sort: false,
        customBodyRender: (value: any, tableMeta: any) => {
          return (
            <div>
              <span>{value.name}</span>
            </div>
          );
        },
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
    rowsPerPage: 3,
    selectableRows: 'none' as const,
    setTableProps: () => {
      return {
        size: 'small',
      };
    },
  };
  return (
    <div>
      <div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Add New Sub Categories</h3>
            <span className={Styles.content}>
              Manage your raw materials (Raw, Semi Furnished & Finished)
            </span>
          </div>
          <form onSubmit={formik.handleSubmit}>
            <div className={Styles.fields}>
              <div>
                <InputLabel
                  id="category_id-label"
                  sx={{
                    fontSize: '0.75rem',
                    fontWeight: '400',
                    color: '#333C44',
                    marginBottom: '4px',
                    marginTop: '-15px',
                  }}
                >
                  Category
                </InputLabel>
                <Select
                  name="category_id"
                  size="small"
                  placeholder="Select the category"
                  sx={{
                    width: '200px',
                    padding: '0 12px 0 12px',
                    backgroundColor: '#f4f5f6',
                  }}
                  value={formik.values.category_id}
                  onChange={formik.handleChange}
                >
                  {getAllCategory &&
                    getAllCategory.map((option: any) => (
                      <MenuItem
                        key={option.category_id}
                        value={option.category_id}
                      >
                        {option.name}
                      </MenuItem>
                    ))}
                </Select>
                {
                  <div
                    style={{
                      color: 'red',
                      marginTop: '2px',
                      fontSize: '0.75rem',
                    }}
                  >
                    {formik.errors.category_id}
                  </div>
                }
              </div>
              <Input
                name="name"
                label="Sub Category Name"
                placeholder="Enter sub category name"
                value={formik.values.name}
                onChange={formik.handleChange}
                error={formik.touched.name && formik.errors.name}
                width="260px"
              />
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
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                >
                  Add New Sub Category
                </Button>
              </div>
            </div>
          </form>
        </div>
        <div className={Styles.box}>
          <div className={Styles.tableContainer}>
            <div className={Styles.textContent}>
              <h3>List of Sub Categories</h3>
              <span className={Styles.content}>
                Manage your raw materials (Raw, Semi Furnished & Finished)
              </span>
            </div>
            <div>
              <div className={Styles.searchContainer}>
                <Input
                  width="260px"
                  prefixIcon={<SearchIcon />}
                  name="search_by_name"
                  onChange={(e) => handleFilterChange(e)}
                  placeholder="Search by item name"
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
                >
                  Reset
                </Button>
              </div>
            </div>
            <div>
              <div>
                <MUIDataTable
                  title={'Sub Category List'}
                  columns={columns}
                  options={options}
                  data={getAllSubCategory}
                />
              </div>
            </div>
          </div>
        </div>
      </div>
      <CustomDialog
        open={openDelete}
        handleClose={handleCloseDelete}
        title="Delete Sub Category"
        content="Are you want to delete this Sub Category?"
        handleConfirm={deleteSubcategory}
      />
      <MySnackbar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        severity={'success'}
        autoHideDuration={1000}
      />
      <CustomDialogBox
        open={open}
        handleClose={handleClose}
        title="Sub Category Form"
        content={
          <CategoryForm
            setOpen={setOpen}
            open={open}
            setReload={setReload}
            mode={mode}
            subCategoryId={subCategoryId}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
          />
        }
      />
    </div>
  );
};

export default SubCategoryList;
