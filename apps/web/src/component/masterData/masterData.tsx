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
    isLoading: FilterLoading,
  } = getBySearchmasterData();
  const { data: getAllmasterData, isLoading: getAllloading } =
    useGetAllmasertData();
  const { data: getAllmasterDataForDrop = [], isLoading: dropLoading } =
    useGetAllParentmasertDataDrop();
  console.log('dropLoading', dropLoading);

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
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const searchValue = event.target.value;
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
    setIsResetDisabled(searchValue === '');
  };
  useEffect(() => {
    handleSearch();
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
    await postDataForFilter(masterData);
    setTotalPages(getFilterData?.total_page);
    setIsLoading(false);
    setFilter(true);
  };
  const handleReset = async () => {
    setFilterValues({
      search_by_name: '',
    });
    setSelectedValue('');
    const masterData: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'asc',
      status: 'AC',
      global_search: '',
    };
    postDataForFilter(masterData);
    setIsLoading(false);
    setFilter(false);
    setIsLoading(false);
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
              setMessage('Master Data has added successfully');
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

  const countries: Option[] = [
    { label: 'PROJECT TYPE', value: 81 },

    { label: 'DESIGNATION', value: 73 },

    { label: 'PURPOSE', value: 72 },

    { label: 'DEPARTMENT', value: 71 },
    { label: 'SITE EXPANSE DESCRIPTION ', value: 70 },
    // ... other countries ...
  ];
  return (
    <div>
      <CustomLoader loading={FilterLoading} size={48} color="#333C44">
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
                      onSelect={(value) =>
                        formik.setFieldValue('parent_master_data_id', value)
                      }
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
                      onClick={() => console.log(formik.values)}
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
                <SelectNew
                  name="parent_master_data_id"
                  onChange={handleDropdownChange}
                  value={selectedValue}
                  defaultLabel="Select from options"
                  width="300px"
                >
                  {getAllmasterDataForDrop.map((option: any) => (
                    <option key={option.value} value={option.value}>
                      {option.label}
                    </option>
                  ))}
                </SelectNew>
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
                    {getFilterData?.total_count === 0 ? (
                      <tr>
                        <td></td>
                        <td></td>
                        <td>No data found</td>
                        <td></td>
                        {activeButton === 'AC' && <td></td>}
                      </tr>
                    ) : (
                      ''
                    )}
                    {getFilterData?.content?.map((item: any, index: number) => (
                      <tr key={item.master_data_id}>
                        <td>{index + 1}</td>
                        <td>{item.master_data_name}</td>
                        <td>{item.master_data_description}</td>
                        <td>{item.master_data_type}</td>
                        <td>
                          {item?.parent?.master_data_name === undefined
                            ? '-'
                            : item?.parent?.master_data_name}
                        </td>
                        {activeButton === 'AC' && (
                          <td>
                            <EditIcon
                              onClick={() => handleEdit(item.master_data_id)}
                            />
                          </td>
                        )}
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
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
