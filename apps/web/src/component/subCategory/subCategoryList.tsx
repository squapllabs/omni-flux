import React, { useState, useEffect } from 'react';
import Styles from '../../styles/subCategoryList.module.scss';
import {
  useDeleteSubcategory,
  useGetBySearchCategroy,
} from '../../hooks/subCategory-hooks';
import CategoryForm from './SubCategoryForm';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
// import { useFormik } from 'formik';
// import { getCreateValidateyup } from '../../helper/constants/category/subcategory-constants';
// import * as Yup from 'yup';
// import { useCreateSubcategory } from '../../hooks/subCategory-hooks';
// import { useGetAllCategoryForDrop } from '../../hooks/category-hooks';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomGroupButton from '../ui/CustomGroupButton';
import EditIcon from '../menu/icons/editIcon';
// import DeleteIcon from '../menu/icons/deleteIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import CustomEditDialog from '../ui/customEditDialogBox';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate } from 'react-router-dom';

//Function for SubCategoryList
const SubCategoryList = () => {
  const { mutate: getDeleteSubcategoryByID } = useDeleteSubcategory();
  const {
    mutate: postDataForFilter,
    data: filterBasedData,
    isLoading: filterDataLoading,
  } = useGetBySearchCategroy();
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [filter, setFilter] = useState(false);
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [openDelete, setOpenDelete] = useState(false);
  const [open, setOpen] = useState(false);
  const [subCategoryId, setSubcategoryID] = useState();
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isLoading, setIsLoading] = useState(true);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const navigate = useNavigate();

  /* Function for button group(Active and Inactive) */
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  /* Function for invoking delete sub category popup */
  // const deleteCategoryHandler = (id: any) => {
  //   setValue(id);
  //   setOpenDelete(true);
  // };
  /* Function for closing popup */
  // const handleClose = () => {
  //   setOpen(false);
  // };
  /* Function for closing delete popup */
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  /* Function for editing the sub category data */
  const handleEdit = (id: any) => {
    navigate(`/subcategory-edit/${id}`);
  };
  /* Function for closing the snackbar */
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  /* Function for deleteing the sub category data */
  const deleteSubcategory = () => {
    getDeleteSubcategoryByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  /* Function for searching from the sub category list */
  const handleSearch = async () => {
    const subcategoryData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      ...filterValues,
    };
    postDataForFilter(subcategoryData);
    setIsLoading(false);
    setFilter(true);
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

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
  /* Function for resting the search and data to normal state */
  const handleReset = async () => {
    const subcategoryData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(subcategoryData);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
    setIsResetDisabled(true);
  };

  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      handleReset();
    }
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div>
      <CustomLoader loading={filterDataLoading} size={48} color="#333C44">
        <div>
          <div className={Styles.top}>
            <div className={Styles.textContent}>
              <h3>Add New Sub Categories</h3>
            </div>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon color="white" />}
                onClick={() => {
                  navigate('/subcategory-add');
                }}
              >
                Add Sub Category
              </Button>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.box}>
            <div className={Styles.tableContainer}>
              <div className={Styles.textContent1}>
                <h3>List of Sub Categories</h3>
              </div>
              <div>
                <div className={Styles.searchContainer}>
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
                      disabled={isResetDisabled}
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
              </div>
              <div>
                <div className={Styles.tableContainer}>
                  <div>
                    <table>
                      <thead>
                        <tr>
                          <th>S No</th>
                          <th>Category</th>
                          <th>Sub Category Name</th>
                          <th>Budget</th>
                          <th>Description</th>
                          {activeButton === 'AC' && <th>Actions</th>}
                        </tr>
                      </thead>
                      <tbody>
                        {filterBasedData?.total_count === 0 ? (
                          <tr>
                            <td></td>
                            <td></td>
                            <td>No data found</td>
                            {activeButton === 'AC' && <td></td>}
                          </tr>
                        ) : (
                          ''
                        )}
                        {filterBasedData?.content?.map(
                          (item: any, index: number) => (
                            <tr key={item.sub_category_id}>
                              <td>{startingIndex + index}</td>
                              <td>{item.category.name}</td>
                              <td>{item.name}</td>
                              <td>{formatBudgetValue(item.budget)}</td>
                              <td>
                                <span title={item.description}>
                                  {item.description
                                    ? item.description.substring(0, 20)
                                    : '-'}
                                </span>
                              </td>
                              {activeButton === 'AC' && (
                                <td>
                                  <div className={Styles.tableIcon}>
                                    <div>
                                      <EditIcon
                                        onClick={() =>
                                          handleEdit(item.sub_category_id)
                                        }
                                      />
                                    </div>
                                    {/* <div>
                                              <DeleteIcon
                                                onClick={() =>
                                                  deleteCategoryHandler(
                                                    item.sub_category_id
                                                  )
                                                }
                                              />
                                            </div> */}
                                  </div>
                                </td>
                              )}
                            </tr>
                          )
                        )}
                      </tbody>
                    </table>
                  </div>
                  <div className={Styles.pagination}>
                    <Pagination
                      currentPage={currentPage}
                      totalPages={filterBasedData?.total_page}
                      totalCount={filterBasedData?.total_count}
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
        <CustomDelete
          open={openDelete}
          title="Delete"
          contentLine1="Are you sure you want to delete this Sub-Category ?"
          contentLine2=""
          handleClose={handleCloseDelete}
          handleConfirm={deleteSubcategory}
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomEditDialog
          open={open}
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
