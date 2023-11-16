import React, { useEffect, useState } from 'react';
import Styles from '../../styles/categoryList.module.scss';
import {
  useDeleteCategory,
  useGetBySearchCategroy,
} from '../../hooks/category-hooks';
import CategoryForm from './categoryForm';
import Button from '../ui/Button';
import Input from '../../component/ui/Input';
import SearchIcon from '../menu/icons/search';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomDelete from '../ui/customDeleteDialogBox';
import EditIcon from '../menu/icons/editIcon';
import CustomSnackBar from '../ui/customSnackBar';
import CustomEditDialog from '../ui/customEditDialogBox';
import AddIcon from '../menu/icons/addIcon';
import { formatBudgetValue } from '../../helper/common-function';
import { useNavigate } from 'react-router-dom';
import CustomBomAddPopup from '../ui/CustomBomAddPopup';
/* Function for  CategoryList */
import DataTable from '../ui/Table';
interface Column {
  field: string;
  label: string;
  render?: (value: any, row: any) => JSX.Element;
}

const CategoryList = () => {
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = useGetBySearchCategroy();
  const { mutate: getDeleteCategoryByID } = useDeleteCategory();
  const navigate = useNavigate();
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [categoryId, setCategoryID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [isLoading, setIsLoading] = useState(true);
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [filter, setFilter] = useState(false);

  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  // const [showClientForm, setShowClientForm] = useState(false);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('AC');

  /* Function for Filter Change */
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

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage, activeButton]);

  /* Function for search */
  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      ...filterValues,
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(true);
  };

  /* Function for resting the search field and data to normal state */
  const handleReset = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      search_by_name: '',
    });
    setIsLoading(false);
    setIsResetDisabled(true);
  };

  /* Function for changing the table page */
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

  // const deleteCategoryHandler = (id: any) => {
  //   setValue(id);
  //   setOpenDelete(true);
  // };

  /* Function for closing the delete popup */
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  /* Function for editing the Category */
  const handleEdit = (id: any) => {
    navigate(`/category-edit/${id}`);
  };

  /* Function for closing the snackbar */
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  /* Function for deleting a category */
  const deleteCategory = () => {
    getDeleteCategoryByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  /* Function for group button (Active and Inactive status) */
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  const columns: Column[] = [
    { field: 'category_id', label: 'Category ID' },
    { field: 'name', label: 'Name' },
    { field: 'budget', label: 'Budget' },
    { field: 'description', label: 'Description' },

    {
      field: 'edit',
      label: 'Actions',
      render: (value: any, row: any) => (
        <div className={Styles.tableIcon}>
          <div>
            <EditIcon onClick={() => handleEdit(row.category_id)} />
          </div>
        </div>
      ),
    },
  ];

  const handleCellClick = (value: any, field: string, rowIndex: number) => {
    console.log(
      `You clicked on row ${rowIndex}, field ${field}, value ${value}`
    );
  };

  return (
    <div>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
        <div>
          <div className={Styles.top}>
            <div className={Styles.textContent}>
              <h3>Add New Categories</h3>
              {/* <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon />}
                onClick={() => {
                 setShowClientForm(true);
                }}
              >
                Items
              </Button> */}
            </div>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                icon={<AddIcon color="white" />}
                onClick={() => {
                  navigate('/category-add');
                }}
              >
                Add Category
              </Button>
            </div>
          </div>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.box}>
            <div className={Styles.tableContainer}>
              <div className={Styles.textContent1}>
                <h3>List of Categories</h3>
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
              <div className={Styles.tableContainer}>
                <div>
                  {getFilterData && getFilterData.content ? (
                    <DataTable
                      columns={columns}
                      data={getFilterData.content}
                      onCellClick={handleCellClick}
                    />
                  ) : (
                    <div>Loading...</div>
                  )}
                </div>
                <div className={Styles.pagination}>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={getFilterData?.total_page}
                    totalCount={getFilterData?.total_count}
                    rowsPerPage={rowsPerPage}
                    onPageChange={handlePageChange}
                    onRowsPerPageChange={handleRowsPerPageChange}
                  />
                </div>
              </div>
            </div>
          </div>
        </div>
      </CustomLoader>
      <CustomEditDialog
        open={open}
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
      <CustomDelete
        open={openDelete}
        title="Delete Category"
        contentLine1="Are you sure you want to delete this Category ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteCategory}
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
      {/* <CustomBomAddPopup
          isVissible={showClientForm}
          onAction={setShowClientForm}
        /> */}
    </div>
  );
};

export default CategoryList;
