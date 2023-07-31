import React, { useState, useEffect } from 'react';
import Styles from '../../styles/subSubCategoryList.module.scss';
import MUIDataTable from 'mui-datatables';
import { Tooltip, IconButton } from '@mui/material';
import DeleteIcon from '@mui/icons-material/Delete';
import EditIcon from '@mui/icons-material/Edit';
import MySnackbar from '../ui/MySnackbar';
import {
  useGetAllSubSubcategory,
  useDeleteSubSubcategory,
  getBySearchSubSubCategroy,
} from '../../hooks/subSubCategory-hooks';
import CustomDialog from '../ui/customDialog';
import SubSubForm from './subSubForm';
import CustomDialogBox from '../ui/cusotmDialogDelete';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import { useFormik } from 'formik';
import { useGetAllSubcategoryDrop } from '../../hooks/subCategory-hooks';
import { getCreateValidateyup } from '../../helper/constants/category/subsubcategory-constants';
import { createSubSubcategory } from '../../hooks/subSubCategory-hooks';
import * as Yup from 'yup';
import Select from '../ui/Select';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomGroupButton from '../ui/CustomGroupButton';

const SubSubCategoryList = () => {
  const { data: getAllSubSubCategory, isLoading: loader } =
    useGetAllSubSubcategory();
  const {
    mutate: postFilterRequest,
    data: getFilterData,
    isLoading: filterLoading,
  } = getBySearchSubSubCategroy();
  console.log('getFilterData', getFilterData);

  const { data: getAllSubCategory = [] } = useGetAllSubcategoryDrop();
  const { mutate: getDeleteSubSubCategoryByID } = useDeleteSubSubcategory();
  const [value, setValue] = useState(0);
  const [open, setOpen] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [openPopup, setOpenPopup] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [subSubCategoryId, setSubSubCategoryId] = useState();
  const { mutate: createNewSubSubCategory } = createSubSubcategory();
  const validationSchema = getCreateValidateyup(Yup);
  const [selectedValue, setSelectedValue] = useState('');
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
  const [totalPages, setTotalPages] = useState(3); // Set initial value to 1
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'active', value: 'AC' },
    { label: 'inactive', value: 'IC' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const handlePageChange = (page: React.SetStateAction<number>) => {
    setCurrentPage(page);
  };

  const handleRowsPerPageChange = (
    newRowsPerPage: React.SetStateAction<number>
  ) => {
    setRowsPerPage(newRowsPerPage);
    setCurrentPage(1);
  };
  const handleSearch = async () => {
    let demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: activeButton,
      ...filterValues,
    };
    postFilterRequest(demo);
    setIsLoading(false);
    setFilter(true);
  };
  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);
  const deleteSubSubCategoryHandler = (id: any) => {
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
  const handleEdit = (event: React.FormEvent, value: any) => {
    console.log('value', value);
    setMode('EDIT');
    setSubSubCategoryId(value);
    setOpenPopup(true);
  };

  const handleClosePopup = () => {
    setOpenPopup(false);
  };

  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
  };

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
  };

  const handleReset = async () => {
    let demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
    };
    postFilterRequest(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
  };
  const options = {
    filter: false,
    search: true,
    caseSensitive: false,
    print: false,
    download: false,
    viewColumns: false,
    search: false,
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

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          name: values.name,
          budget: Number(values.budget),
          sub_category_id: Number(selectedValue),
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

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  return (
    <div>
      <CustomLoader
        loading={isLoading === true ? loader : filterLoading}
        // loading={true}
        size={48}
        color="#333C44"
      >
        <div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>Add New Sub Sub Categories</h3>
              <span className={Styles.content}>
                Manage your raw materials (Raw, Semi Furnished & Finished).
              </span>
            </div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.fields}>
                <div className={Styles.projectField}>
                  <span className={Styles.projectHeading}>Sub Category</span>
                  <Select
                    options={getAllSubCategory}
                    onChange={handleDropdownChange}
                    value={selectedValue}
                    defaultLabel="Select from options"
                    width="100%"
                  />
                  {formik.touched.sub_category_id &&
                    formik.errors.sub_category_id && (
                      <div className={Styles.error}>
                        {formik.errors.sub_category_id}
                      </div>
                    )}
                </div>
                <div>
                  <Input
                    name="name"
                    label="Sub Sub Category Name"
                    placeholder="Sub sub category name"
                    value={formik.values.name}
                    onChange={formik.handleChange}
                    error={formik.touched.name && formik.errors.name}
                    width="100%"
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
                    Add New Sub Sub Category
                  </Button>
                </div>
              </div>
            </form>
          </div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Sub Sub Categories</h3>
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
                      <th>Sub Category</th>
                      <th>Name</th>
                      <th>Budget</th>
                      <th>Option</th>
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
                    {getFilterData?.content?.map((item: any) => (
                      <tr>
                        <td>{item.sub_category.name}</td>
                        <td>{item.name}</td>
                        <td>{item.budget}</td>
                        <td>
                          <IconButton
                            onClick={(e) =>
                              handleEdit(e, item.sub_sub_category_id)
                            }
                          >
                            <EditIcon />
                          </IconButton>
                          <IconButton
                            onClick={() =>
                              deleteSubSubCategoryHandler(
                                item.sub_sub_category_id
                              )
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
      </CustomLoader>
    </div>
  );
};

export default SubSubCategoryList;
