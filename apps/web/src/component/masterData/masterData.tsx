import React, { useState, useEffect, useRef } from 'react';
import Styles from '../../styles/masterdata.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getCreateValidateyup } from '../../helper/constants/master-constants';
import CustomSnackBar from '../ui/customSnackBar';
import {
  useGetAllmasertData,
  createmasertData,
  useGetAllParentmasertDataDrop,
  useDeletemasertData,
  getBySearchmasterData,
  useGetAllPaginatedMasterData,
} from '../../hooks/masertData-hook';
import EditIcon from '../menu/icons/editIcon';
import SearchIcon from '../menu/icons/search';
import CustomEditDialog from '../ui/customEditDialogBox';
import CustomDelete from '../ui/customDeleteDialogBox';
import MasterDataEditForm from './masterDataEditForm';
import Pagination from '../menu/pagination';
import CustomLoader from '../ui/customLoader';
import SelectNew from '../ui/selectNew';
import AddIcon from '../menu/icons/addIcon';
import TextArea from '../ui/CustomTextArea';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';

const MaterData = () => {
  const [selectedValue, setSelectedValue] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [initialValues, setInitialValues] = useState({
    master_data_id: '',
    master_data_name: '',
    master_data_description: '',
    master_data_type: '',
    parent_master_data_id: '',
  });
  const [value, setValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [categoryId, setCategoryID] = useState();
  const [mode, setMode] = useState('');
  const [open, setOpen] = useState(false);
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const [filterValues, setFilterValues] = useState({
    search_by_name: '',
  });
  const [isLoading, setIsLoading] = useState(true);
  const [filter, setFilter] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(3); // Set initial value to 1
  const [rowsPerPage, setRowsPerPage] = useState(5);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [reload, setReload] = useState(false);
  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: searchLoader,
  } = getBySearchmasterData();
  const { data: getAllmasterData, isLoading: getAllloading } =
    useGetAllmasertData();
  const { data: getAllmasterDataForDrop = [], isLoading: dropLoading } =
    useGetAllParentmasertDataDrop();
  const { mutate: postMasterData } = createmasertData();
  const { mutate: getDeleteMasterDataID } = useDeletemasertData();
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;
    setFilterValues({
      ...filterValues,
      ['search_by_name']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
  };
  const [dataShow, setDataShow] = useState(false);
  const masterData = {
    limit: rowsPerPage,
    offset: (currentPage - 1) * rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    global_search: filterValues.search_by_name,
  };
  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedMasterData(masterData);
  // console.log("master data===>",initialData);

  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
    setIsResetDisabled(searchValue === '');
  };
  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);
  const handleSearch = async () => {
    const masterData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: activeButton,
      global_search: filterValues.search_by_name,
      parent_id: Number(selectedValue),
    };
    postDataForFilter(masterData);
    setDataShow(true);
    // setTotalPages(getFilterData?.total_page);
    setIsLoading(false);
    setFilter(true);
  };
  const handleReset = async () => {
    setFilterValues({
      search_by_name: '',
    });
    // const masterData: any = {
    //   offset: (currentPage - 1) * rowsPerPage,
    //   limit: rowsPerPage,
    //   order_by_column: 'updated_date',
    //   order_by_direction: 'asc',
    //   status: 'AC',
    //   global_search: '',
    // };
    // postDataForFilter(masterData);
    setSelectedValue('');
    setDataShow(false);
    setIsLoading(false);
    setFilter(false);
    setIsLoading(false);
    setSelectedValue('');
    setIsResetDisabled(true);
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
  const validationSchema = getCreateValidateyup(Yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        let object: any = {};
        const num = 0;
        if (Number(values.parent_master_data_id) === num) {
          object = {
            master_data_name: values.master_data_name,
            master_data_description: values.master_data_description,
            master_data_type: values.master_data_type,
            parent_master_data_id: null,
          };
        } else {
          object = {
            master_data_name: values.master_data_name,
            master_data_description: values.master_data_description,
            master_data_type: values.master_data_type,
            parent_master_data_id: Number(values.parent_master_data_id),
          };
        }

        postMasterData(object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Master Data created');
              setOpenSnack(true);
              setSelectedValue('');
              resetForm();
            }
          },
        });
      }
    },
  });
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const deleteCategoryHandler = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };
  const handleEdit = (value: any) => {
    setMode('EDIT');
    setCategoryID(value);
    setOpen(true);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const deleteCategory = () => {
    getDeleteMasterDataID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;
  return (
    <div>
      <CustomLoader
        loading={searchLoader ? searchLoader : getAllLoadingPaginated}
        size={48}
        color="#333C44"
      >
        <div className={Styles.conatiner}>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>Add New Master Data</h3>
              <span className={Styles.content}>
                Manage your master data across your application
              </span>
            </div>
            <form onSubmit={formik.handleSubmit}>
              <div className={Styles.fields_container}>
                <div className={Styles.fields_container_1}>
                  <div className={Styles.inputField}>
                    <Input
                      name="master_data_name"
                      label="Name"
                      placeholder="Enter master name"
                      value={formik.values.master_data_name}
                      onChange={formik.handleChange}
                      mandatory={true}
                      error={
                        formik.touched.master_data_name &&
                        formik.errors.master_data_name
                      }
                    />
                  </div>
                  <div>
                    <Input
                      name="master_data_type"
                      label="Code"
                      placeholder="Enter code"
                      value={formik.values.master_data_type}
                      onChange={formik.handleChange}
                      mandatory={true}
                      error={
                        formik.touched.master_data_type &&
                        formik.errors.master_data_type
                      }
                    />
                  </div>
                  <div>
                    <AutoCompleteSelect
                      label="Parent Name"
                      name="parent_master_data_id"
                      onChange={formik.handleChange}
                      value={formik.values.parent_master_data_id}
                      placeholder="Select from options"
                      width="200px"
                      onSelect={(value) => {
                        formik.setFieldValue('parent_master_data_id', value);
                      }}
                      optionList={
                        dropLoading === true ? [] : getAllmasterDataForDrop
                      }
                      error={
                        formik.touched.parent_master_data_id &&
                        formik.errors.parent_master_data_id
                      }
                    />
                  </div>
                </div>
                <div className={Styles.fields_container_2}>
                  <div className={Styles.inputField}>
                    <TextArea
                      name="master_data_description"
                      label="Description"
                      placeholder="Enter description"
                      value={formik.values.master_data_description}
                      onChange={formik.handleChange}
                      mandatory={true}
                      error={
                        formik.touched.master_data_description &&
                        formik.errors.master_data_description
                      }
                      rows={3}
                      maxCharacterCount={120}
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
                      Add
                    </Button>
                  </div>
                </div>
              </div>
            </form>
          </div>
          <div className={Styles.box}>
            <div className={Styles.textContent}>
              <h3>List of Master Data</h3>
              <span className={Styles.content}>
                Manage your master data across your application
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
                  placeholder="Search by name"
                />
                <AutoCompleteSelect
                  name="parent_master_data_id"
                  defaultLabel="Select Parent Name"
                  onChange={() => handleDropdownChange}
                  value={selectedValue}
                  placeholder="Parent Name"
                  width="260px"
                  onSelect={(value) => {
                    setSelectedValue(value);
                    setIsResetDisabled(false);
                  }}
                  optionList={
                    dropLoading === true ? [] : getAllmasterDataForDrop
                  }
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
                  disabled={isResetDisabled}
                  onClick={handleReset}
                >
                  Reset
                </Button>
              </div>
            </div>
            <div className={Styles.tableContainer}>
              <div>
                <table>
                  <thead>
                    <tr>
                      <th>S No</th>
                      <th>Name</th>
                      <th>Description</th>
                      <th>Code</th>
                      <th>Parent Name</th>
                      {activeButton === 'AC' && <th></th>}
                    </tr>
                  </thead>
                  <tbody>
                    {dataShow ? (
                      getFilterData?.total_count === 0 ? (
                        <tr>
                          <td></td>
                          <td></td>
                          <td>No data found</td>
                          <td></td>
                          {activeButton === 'AC' && <td></td>}
                        </tr>
                      ) : (
                        getFilterData?.content?.map(
                          (data: any, index: number) => (
                            <tr key={data.master_data_id}>
                              {/* <td>{index + 1}</td> */}
                              <td>{startingIndex + index}</td>
                              <td>{data.master_data_name}</td>
                              <td>{data.master_data_description}</td>
                              <td>{data.master_data_type}</td>
                              <td>
                                {data?.parent?.master_data_name === undefined
                                  ? '-'
                                  : data?.parent?.master_data_name}
                              </td>
                              {activeButton === 'AC' && (
                                <td>
                                  <EditIcon
                                    onClick={() =>
                                      handleEdit(data.master_data_id)
                                    }
                                  />
                                </td>
                              )}
                            </tr>
                          )
                        )
                      )
                    ) : initialData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        {activeButton === 'AC' && <td></td>}
                      </tr>
                    ) : (
                      initialData?.content?.map((data: any, index: number) => (
                        <tr key={data.uom_id}>
                          <td>{startingIndex + index}</td>
                          <td>{data.master_data_name}</td>
                          <td>{data.master_data_description}</td>
                          <td>{data.master_data_type}</td>
                          <td>
                            {data?.parent?.master_data_name === undefined
                              ? '-'
                              : data?.parent?.master_data_name}
                          </td>
                          {activeButton === 'AC' && (
                            <td>
                              <EditIcon
                                onClick={() => handleEdit(data.master_data_id)}
                              />
                            </td>
                          )}
                        </tr>
                      ))
                    )}
                  </tbody>
                </table>
              </div>
            </div>
            <div className={Styles.pagination}>
              <Pagination
                currentPage={currentPage}
                totalPages={
                  dataShow ? getFilterData?.total_page : initialData?.total_page
                }
                totalCount={
                  dataShow
                    ? getFilterData?.total_count
                    : initialData?.total_count
                }
                rowsPerPage={rowsPerPage}
                onPageChange={handlePageChange}
                onRowsPerPageChange={handleRowsPerPageChange}
              />
            </div>
          </div>
        </div>
        <CustomSnackBar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
        <CustomDelete
          open={openDelete}
          handleClose={handleCloseDelete}
          title="Delete Category"
          contentLine1="Are you want to delete this Category?"
          contentLine2=""
          handleConfirm={deleteCategory}
        />
        <CustomEditDialog
          open={open}
          content={
            <MasterDataEditForm
              setOpen={setOpen}
              open={open}
              setReload={setReload}
              mode={mode}
              masterID={categoryId}
              setOpenSnack={setOpenSnack}
              setMessage={setMessage}
            />
          }
        />
      </CustomLoader>
    </div>
  );
};

export default MaterData;
