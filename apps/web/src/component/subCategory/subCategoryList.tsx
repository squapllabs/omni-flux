import React, { useState, useEffect } from 'react';
import Styles from '../../styles/subCategoryList.module.scss';
import MUIDataTable from 'mui-datatables';
import { Tooltip, IconButton, InputLabel } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import {
  useGetAllSubcategory,
  useDeleteSubcategory,
  getBySearchCategroy,
} from '../../hooks/subCategory-hooks';
import CategoryForm from './SubCategoryForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import CustomDialog from '../ui/customDialog';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import { useFormik } from 'formik';
import { getCreateValidateyup } from '../../helper/constants/category/subcategory-constants';
import * as Yup from 'yup';
import { createSubcategory } from '../../hooks/subCategory-hooks';
import { useGetAllCategoryForDrop } from '../../hooks/category-hooks';
import SearchIcon from '../menu/icons/search';
import SelectDrop from '../ui/Select';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';

/**
 * Function for SubCategoryList
 */
const SubCategoryList = () => {
  const validationSchema = getCreateValidateyup(Yup);
  const { data: getAllSubCategory, isLoading: getAllDataLoading } =
    useGetAllSubcategory();
  const { data: getAllCategoryDrop = [] } = useGetAllCategoryForDrop();
  const { mutate: getDeleteSubcategoryByID } = useDeleteSubcategory();
  const { mutate: createNewSubcategory } = createSubcategory();
  const {
    mutate: postDataForFilter,
    data: filterBasedData,
    isLoading: filterDataLoading,
  } = getBySearchCategroy();

  const [initialValues, setInitialValues] = useState({
    sub_category_id: '',
    name: '',
    budget: '',
  });
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [filter, setFilter] = useState(false);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        name: values.name,
        budget: Number(values.budget),
        category_id: Number(selectedValue),
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
  const [selectedValue, setSelectedValue] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(3); // Set initial value to 1
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const [isLoading, setIsLoading] = useState(true);
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
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedCategoryId = event.target.value;
    setSelectedValue(selectedCategoryId);
  };
  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);
  const handleSearch = async () => {
    let demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'AC',
      ...filterValues,
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(true);
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
  const handleReset = async () => {
    let demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
  };
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };

  return (
    <div>
      <CustomLoader
        loading={isLoading === true ? getAllDataLoading : filterDataLoading}
        // loading={true}
        size={48}
        color="#333C44"
      >
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
                      marginTop: '-20px',
                    }}
                  >
                    Category
                  </InputLabel>
                  <SelectDrop
                    options={getAllCategoryDrop}
                    onChange={handleDropdownChange}
                    value={selectedValue}
                    defaultLabel="Select from options"
                    width="100%"
                  />
                  {
                    <div
                      style={{
                        color: 'red',
                        marginTop: '2px',
                        fontSize: '0.75rem',
                      }}
                    >
                      {/* {formik.errors.category_id} */}
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
                    value={filterValues.search_by_name}
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
                    onClick={handleReset}
                  >
                    Reset
                  </Button>
                </div>
              </div>
              <div>
                <div className={Styles.tableContainer}>
                  <div>
                    <table>
                      <thead>
                        <tr>
                          <th>Category</th>
                          <th>Name</th>
                          <th>Budget</th>
                          <th>Option</th>
                        </tr>
                      </thead>
                      <tbody>
                        {filterBasedData?.total_count === 0 ? (
                          <tr>
                            <td></td>
                            <td></td>
                            <td>No data found</td>
                            <td></td>
                          </tr>
                        ) : (
                          ''
                        )}
                        {filterBasedData?.content?.map((item: any) => (
                          <tr>
                            <td>{item.category.name}</td>
                            <td>{item.name}</td>
                            <td>{item.budget}</td>
                            <td>
                              <IconButton
                                onClick={(e) => handleEdit(e, item.category_id)}
                              >
                                <EditIcon />
                              </IconButton>
                              <IconButton
                                onClick={() =>
                                  deleteCategoryHandler(item.category_id)
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
                  <div className={Styles.pagination}>
                    <Pagination
                      currentPage={currentPage}
                      totalPages={totalPages}
                      rowsPerPage={rowsPerPage}
                      onPageChange={handlePageChange}
                      onRowsPerPageChange={handleRowsPerPageChange}
                    />
                  </div>
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
      </CustomLoader>
    </div>
  );
};

export default SubCategoryList;
