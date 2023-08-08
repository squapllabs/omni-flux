import React, { useState, useEffect } from 'react';
import Styles from '../../styles/subSubCategoryList.module.scss';
import {
  useDeleteSubSubcategory,
  getBySearchSubSubCategroy,
} from '../../hooks/subSubCategory-hooks';
import SubSubForm from './subSubForm';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import { useFormik } from 'formik';
import { useGetAllSubcategoryDrop } from '../../hooks/subCategory-hooks';
import { getCreateValidateyup } from '../../helper/constants/category/subsubcategory-constants';
import { createSubSubcategory } from '../../hooks/subSubCategory-hooks';
import * as Yup from 'yup';
import Select from '../ui/selectNew';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomGroupButton from '../ui/CustomGroupButton';
import EditIcon from '../menu/icons/editIcon';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import CustomEditDialog from '../ui/customEditDialogBox';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';

/* Function for sub sub category */
const SubSubCategoryList = () => {
  const {
    mutate: postFilterRequest,
    data: getFilterData,
    isLoading: filterLoading,
  } = getBySearchSubSubCategroy();
  const { data: getAllSubCategory = [] } = useGetAllSubcategoryDrop();
  const { mutate: getDeleteSubSubCategoryByID } = useDeleteSubSubcategory();
  const { mutate: createNewSubSubCategory } = createSubSubcategory();
  const [value, setValue] = useState(0);
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openPopup, setOpenPopup] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [subSubCategoryId, setSubSubCategoryId] = useState();
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [initialValues, setInitialValues] = useState({
    sub_sub_category_id: '',
    name: '',
    budget: '',
    sub_category_id: '',
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const validationSchema = getCreateValidateyup(Yup);

  /* Function for changing page in table */
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };
  /* Function for changing no of rows in pagination */
  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  /* Function for searching */
  const handleSearch = async () => {
    const subsubcategoryData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      ...filterValues,
    };
    postFilterRequest(subsubcategoryData);
    setIsLoading(false);
    setFilter(true);
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  /* Function for opening delete popup */
  const deleteSubSubCategoryHandler = (id: any) => {
    setValue(id);
    setOpen(true);
  };
  /* Function for closing popup */
  const handleClose = () => {
    setOpen(false);
  };
  /* Function for deleting sub sub category */
  const deleteSubSubCategory = () => {
    getDeleteSubSubCategoryByID(value);
    handleClose();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };
  /* Function for closing snackbar */
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  /* Function for editing the sub sub category data */
  const handleEdit = (value: any) => {
    setMode('EDIT');
    setSubSubCategoryId(value);
    setOpenPopup(true);
  };
  /* Function for closing delete popup */
  const handleClosePopup = () => {
    setOpenPopup(false);
  };
  /* Function for changing the filter values */
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };
  /* Function for resting the search and data to normal state */
  const handleReset = async () => {
    const subsubcategoryData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postFilterRequest(subsubcategoryData);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
  };
  /* Function for adding to data to sub sub category */
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          sub_category_id: Number(values.sub_category_id),
        };
        createNewSubSubCategory(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success === true) {
              setMessage('Category created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });
  /* Function for group button (Active and Inactive) */
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  return (
    <div>
      <CustomLoader loading={filterLoading} size={48} color="#333C44">
        <div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>Add New Sub Sub Categories</h3>
              {/* <span className={Styles.content}>
                Manage your raw materials (Raw, Semi Furnished & Finished).
              </span> */}
            </div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.fields}>
                <div style={{width:'20%'}}>
                  <Select
                    label="Sub Category"
                    name="sub_category_id"
                    onChange={formik.handleChange}
                    value={formik.values.sub_category_id}
                    defaultLabel="Select from options"
                    error={
                      formik.touched.sub_category_id &&
                      formik.errors.sub_category_id
                    }
                  >
                    {getAllSubCategory.map((option: any) => (
                      <option key={option.value} value={option.value}>
                        {option.label}
                      </option>
                    ))}
                  </Select>
                </div>
                <div>
                  <Input
                    name="name"
                    label="Sub Sub Category Name"
                    placeholder="Sub sub category name"
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
                    icon={<AddIcon />}
                  >
                    Add New Sub Sub Category
                  </Button>
                </div>
              </div>
            </form>
          </div>
          <div className={Styles.box}>
            <div className={Styles.tableContainer}>
              <div className={Styles.textContent}>
                <h3>List of Sub Sub Categories</h3>
                {/* <span className={Styles.content}>
                Manage your raw materials (Raw, Semi Furnished & Finished).
              </span> */}
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
                        <th>S No</th>
                        <th>Sub Category</th>
                        <th>Name</th>
                        <th>Budget</th>
                        <th></th>
                      </tr>
                    </thead>
                    <tbody>
                      {getFilterData?.total_count === 0 ? (
                        <tr>
                          <td></td>
                          <td></td>
                          <td>No data found</td>
                          <td></td>
                        </tr>
                      ) : (
                        ''
                      )}
                      {getFilterData?.content?.map(
                        (item: any, index: number) => (
                          <tr key={item.sub_sub_category_id}>
                            <td>{index + 1}</td>
                            <td>{item.sub_category.name}</td>
                            <td>{item.name}</td>
                            <td>{formatBudgetValue(item.budget)}</td>
                            <td>
                              <div className={Styles.tableIcon}>
                                <div>
                                  <EditIcon
                                    onClick={() =>
                                      handleEdit(item.sub_sub_category_id)
                                    }
                                  />
                                </div>
                                {/* <div>
                              <DeleteIcon
                                onClick={() =>
                                  deleteSubSubCategoryHandler(
                                    item.sub_sub_category_id
                                  )
                                }
                              />
                            </div> */}
                              </div>
                            </td>
                          </tr>
                        )
                      )}
                    </tbody>
                  </table>
                </div>
                <div className={Styles.pagination}>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={getFilterData?.total_count}
                    rowsPerPage={rowsPerPage}
                    onPageChange={handlePageChange}
                    onRowsPerPageChange={handleRowsPerPageChange}
                  />
                </div>
              </div>
            </div>
          </div>
        </div>
        <CustomDelete
          open={open}
          title="Delete"
          contentLine1="Are you sure you want to delete this Sub-Sub-Category ?"
          contentLine2=""
          handleClose={handleClose}
          handleConfirm={deleteSubSubCategory}
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomEditDialog
          open={openPopup}
          // title="Edit Sub Sub Category"
          // subTitle="Please edit the sub sub category"
          // handleClose={handleClosePopup}
          content={
            <SubSubForm
              setOpenPopup={setOpenPopup}
              open={open}
              setReload={setReload}
              mode={mode}
              subSubCategoryId={subSubCategoryId}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
      </CustomLoader>
    </div>
  );
};

export default SubSubCategoryList;
