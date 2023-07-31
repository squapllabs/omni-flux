import React, { useEffect, useState } from 'react';
import Styles from '../../styles/categoryList.module.scss';
import { Tooltip, IconButton } from '@mui/material';
import DeleteIcon from '../menu/icons/deleteIcon';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import {
  useGetAllCategory,
  useDeleteCategory,
  getBySearchCategroy,
} from '../../hooks/category-hooks';
import CategoryForm from './categoryForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import CustomDialog from '../ui/customDialog';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import { useFormik } from 'formik';
import { getCreateValidateyup } from '../../helper/constants/category/category-constants';
import { createCategory } from '../../hooks/category-hooks';
import * as Yup from 'yup';
import Select from '../ui/Select';
import { useGetAllProject } from '../../hooks/project-hooks';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomSwitch from '../ui/customSwitch';
import CustomGroupButton from '../ui/CustomGroupButton';

/**
 * Function for  CategoryList
 */
const CategoryList = () => {
  const { data: getAllCategory, isLoading: getAllLoading } =
    useGetAllCategory();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getBySearchCategroy();
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
  const [isLoading, setIsLoading] = useState(true);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [filter, setFilter] = useState(false);
  const [initialValues, setInitialValues] = useState({
    category_id: '',
    name: '',
    budget: '',
    project_id: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(3); // Set initial value to 1
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const { mutate: createNewCategory } = createCategory();
  const { data: getAllProjectList = [] } = useGetAllProject();
  const [selectedValue, setSelectedValue] = useState('');
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'active', value: 'AC' },
    { label: 'inactive', value: 'IC' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
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
    let demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: AC,
      ...filterValues,
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(true);
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
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
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
            if (data?.message === 'success') {
              setMessage('Category created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  return (
    <div>
      <CustomLoader
        loading={isLoading === true ? getAllLoading : FilterLoading}
        // loading={true}
        size={48}
        color="#333C44"
      >
        <div>
          <div className={Styles.box}>
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
                    <div className={Styles.error}>
                      {formik.errors.project_id}
                    </div>
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
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                  >
                    Add New Category
                  </Button>
                </div>
              </div>
            </form>
          </div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Categories</h3>
              <span className={Styles.content}>
                Manage your raw materials (Raw, Semi Furnished & Finished).
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
                      <th>Name</th>
                      <th>Budget</th>
                      <th>Option</th>
                    </tr>
                  </thead>
                  <tbody>
                    {getFilterData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td>No data found</td>
                        <td></td>
                      </tr>
                    ) : (
                      ''
                    )}
                    {getFilterData?.content?.map((item: any) => (
                      <tr>
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
      </CustomLoader>
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
