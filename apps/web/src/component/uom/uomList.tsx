import React, { useState, useEffect } from 'react';
import Styles from '../../styles/userList.module.scss';
import { IconButton } from '@mui/material';
import DeleteIcon from '../menu/icons/deleteIcon';
import EditIcon from '../menu/icons/editIcon';
import CustomSnackBar from '../ui/customSnackBar';
import { useGetAlluom, useDeleteUom, getByUom } from '../../hooks/uom-hooks';
import UomForm from './uomForm';
import CustomEditDialog from '../ui/customEditDialogBox';
import Button from '../ui/Button';
import { useFormik } from 'formik';
import { createuom } from '../../hooks/uom-hooks';
import Input from '../ui/Input';
import { getuomCreateValidateyup } from '../../helper/constants/uom-constants';
import * as Yup from 'yup';
import CustomGroupButton from '../ui/CustomGroupButton';
import CustomLoader from '../ui/customLoader';
import Pagination from '../menu/pagination';
import SearchIcon from '../menu/icons/search';
import AddIcon from '../menu/icons/addIcon';
import CustomDelete from '../ui/customDeleteDialogBox'

const UomList = () => {
  const { isLoading: getAllLoading } = useGetAlluom();
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getByUom();
  const { mutate: getDeleteuomByID } = useDeleteUom();
  const { mutate: createNewuom } = createuom();
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [uomId, setUomID] = useState();
  const [reload, setReload] = useState(false);
  const [mode, setMode] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(3);
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [value, setValue] = useState();
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [initialValues, setInitialValues] = useState({
    uom_id: '',
    name: '',
    description: '',
  });
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const validationSchema = getuomCreateValidateyup(Yup);

  // const handleClose = () => {
  //   setOpen(false);
  // };

  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const handleEdit = (value: any) => {
    setMode('EDIT');
    setUomID(value);
    setOpen(true);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };

  const deleteUom = () => {
    getDeleteuomByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        const Object: any = {
          uom_id: values.uom_id,
          name: values.name,
          description: values.description,
        };
        createNewuom(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              setMessage('UOM created');
              setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });

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
    const demo: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_by',
      order_by_direction: 'desc',
      status: activeButton,
      global_search: filterValues.search_by_name,
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(true);
  };

  const handleReset = async () => {
    const demo: any = {
      limit: rowsPerPage,
      offset: (currentPage - 1) * rowsPerPage,
      order_by_column: 'updated_by',
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

  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  return (
    <div>
      <div>
        <CustomLoader
          loading={isLoading === true ? getAllLoading : FilterLoading}
          size={48}
          color="#333C44"
        >
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>Add New UOM</h3>
              <span className={Styles.content}>
                Manage your UOM details here.
              </span>
            </div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.fields}>
                <div>
                  <Input
                    label="Unit Of Measurement"
                    placeholder="Enter unit of measurement"
                    name="name"
                    value={formik.values.name}
                    onChange={formik.handleChange}
                    error={formik.touched.name && formik.errors.name}
                    width="100%"
                  />
                </div>
                <div>
                  <Input
                    name="description"
                    label="Description"
                    placeholder="Enter description"
                    value={formik.values.description}
                    onChange={formik.handleChange}
                    error={formik.touched.description && formik.errors.description}
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
                    Add New UOM
                  </Button>
                </div>
              </div>
            </form>
          </div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Unit of Measurements</h3>
              <span className={Styles.content}>
                Manage your UOM details here.
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
                  placeholder="Search"
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
                      <th>UOM Name</th>
                      <th>Description</th>
                      <th>Options</th>
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
                    {getFilterData?.content?.map((data: any, index: number) => (
                      <tr>
                        <td>{index + 1}</td>
                        <td>{data.name}</td>
                        <td>{data.description}</td>
                        <td>
                          <EditIcon onClick={() => handleEdit(data.uom_id)} ></EditIcon>
                          {/* <DeleteIcon onClick={() =>
                            deleteCategoryHandler(data.uom_id)
                          } /> */}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
              <div className={Styles.pagination}>
                <Pagination
                  currentPage={currentPage}
                  totalPages={getFilterData?.total_page}
                  rowsPerPage={rowsPerPage}
                  onPageChange={handlePageChange}
                  onRowsPerPageChange={handleRowsPerPageChange}
                />
              </div>
            </div>
          </div>
        </CustomLoader>
        <CustomEditDialog
          open={open}
          // handleClose={handleClose}
          // title="Edit UOM"
          content={
            <UomForm
              setOpen={setOpen}
              open={open}
              setReload={setReload}
              mode={mode}
              uomId={uomId}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
        <CustomDelete
          open={openDelete}
          title="Delete UOM"
          contentLine1="Are you sure you want to delete this post? This action cannot be undone."
          contentLine2="Deleted UOM will move to Inactive tab."
          handleClose={handleCloseDelete}
          handleConfirm={deleteUom}
        />
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </div>
    </div>
  );
};

export default UomList;
