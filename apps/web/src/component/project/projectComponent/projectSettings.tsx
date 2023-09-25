import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/projectSettings.module.scss';
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import AddIcon from '../../menu/icons/addIcon';
import SearchIcon from '../../menu/icons/search';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import DatePicker from '../../ui/CustomDatePicker';
import CustomGroupButton from '../../ui/CustomGroupButton';
import { useParams, useNavigate } from 'react-router-dom';
import { useGetAllRoles } from '../../../hooks/userRole-hooks';
import {
  createProjectMember,
  getBySearchProjectMembers,
  useGetAllPaginatedProjectMember,
  useDeleteProjectMember,
} from '../../../hooks/projectSettings-hook';
import ProjectSettingsService from '../../../service/projectSettings-service';
import { format } from 'date-fns';
import Avatar from '../../menu/AvatarComponent';
import DeleteIcon from '../../menu/icons/deleteIcon';
import CustomLoader from '../../ui/customLoader';
import Pagination from '../../menu/pagination';
import CustomDelete from '../../ui/customDeleteDialogBox';
import CustomSnackBar from '../../ui/customSnackBar';
import * as Yup from 'yup';
import { getProjectMemberCreationYupschema } from '../../../helper/constants/projectSettings-constants';

const ProjectSettings: React.FC = (props: any) => {
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [isLoading, setIsLoading] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const [filterValues, setFilterValues] = useState({
    global_search: '',
  });
  const { mutate: createNewProjectMember } = createProjectMember();
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const routeParams = useParams();
  const [userData, setUserData] = useState();
  const { data: getAllRolesData = [], isLoading: dropLoading } =
    useGetAllRoles();
  const { mutate: getDeleteProjectMemberByID } = useDeleteProjectMember();

  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [filter, setFilter] = useState(false);
  const [initialValues, setInitialValues] = useState({
    project_role_id: '',
    project_role_name: '',
    user_id: '',
    access_start_date: '',
    access_end_date: '',
    project_id: Number(routeParams?.id),
  });

  const object: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    project_id: Number(routeParams?.id),
    global_search: filterValues.global_search,
  };

  const {
    isLoading: getAllLoadingProjectMemberData,
    data: initialData,
    refetch,
  } = useGetAllPaginatedProjectMember(object);

  const handleSnackBarClose = () => {
    setOpenSnack(false);
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

  const {
    mutate: postDataForFilter,
    data: getFilterData,
    isLoading: FilterLoading,
  } = getBySearchProjectMembers();

  /* Function for search */
  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: activeButton,
      project_id: Number(routeParams?.id),
      ...filterValues,
    };
    postDataForFilter(demo);
    setDataShow(true);
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
      project_id: Number(routeParams?.id),
      global_search: '',
    };
    postDataForFilter(demo);
    setIsLoading(false);
    setFilter(false);
    setFilterValues({
      global_search: '',
    });
    setIsLoading(false);
    setDataShow(false);
    setIsResetDisabled(true);
  };

  /* Function for group button (Active and Inactive status) */
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };

  /* Function for Filter Change */
  const handleFilterChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    const searchValue = event.target.value;

    setFilterValues({
      ...filterValues,
      ['global_search']: event.target.value,
    });
    setIsResetDisabled(searchValue === '');
    if (searchValue === '') {
      handleReset();
    }
  };

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

  const validationSchema = getProjectMemberCreationYupschema(Yup);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        project_id: Number(routeParams?.id),
        project_role_id: values.project_role_id,
        user_id: values.user_id,
        access_start_date: values.access_start_date,
        access_end_date: values.access_end_date,
      };
      createNewProjectMember(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Project Member created');
            setOpenSnack(true);
            // setTimeout(() => {
            //   navigate('/settings');
            // }, 1000);
            resetForm();
          }
        },
      });
    },
  });

  const fetchData = async (data: any) => {
    const roleObj = {
      id: Number(routeParams?.id),
      role: data,
    };
    const getData = await ProjectSettingsService.fetchRoleBasedUser(roleObj);
    let arr: any = [];
    let userList = getData?.data?.map((user: any, index: any) => {
      let obj: any = {
        value: user?.user_id,
        label: user?.first_name + ' ' + user?.last_name,
      };
      arr.push(obj);
    });
    setUserData(arr);
  };

  const deleteProjectMember = (id: any) => {
    setValue(id);
    setOpenDelete(true);
  };

  /* Function for closing the delete popup */
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  /* Function for deleting a category */
  const deleteMember = () => {
    getDeleteProjectMemberByID(value);
    handleCloseDelete();
    setMessage('Successfully deleted');
    setOpenSnack(true);
    handleSearch();
  };
  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.conatiner}>
      <CustomLoader
        loading={FilterLoading ? FilterLoading : getAllLoadingProjectMemberData}
        size={48}
        color="#333C44"
      >
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Invite Member</h3>
          </div>
          <form onSubmit={formik.handleSubmit}>
            <div className={Styles.fields_container}>
              <div className={Styles.fields_container_1}>
                <div>
                  <AutoCompleteSelect
                    label="Permission Role"
                    name="project_role_id"
                    onChange={formik.handleChange}
                    value={formik.values.project_role_id}
                    placeholder="Select from options"
                    width="350px"
                    mandatory
                    onSelect={(value) => {
                      formik.setFieldValue('project_role_id', value);
                      const matchingObjects = getAllRolesData.filter(
                        (obj: any) => Number(obj.value) === Number(value)
                      );
                      formik.setFieldValue(
                        'project_role_name',
                        matchingObjects[0]?.label
                      );
                      fetchData(matchingObjects[0]?.label);
                    }}
                    optionList={dropLoading === true ? [] : getAllRolesData}
                    error={
                      formik.touched.project_role_id &&
                      formik.errors.project_role_id
                    }
                  />
                </div>
                <div>
                  <AutoCompleteSelect
                    label="Members"
                    name="user_id"
                    onChange={formik.handleChange}
                    value={formik.values.user_id}
                    placeholder="Select from options"
                    mandatory
                    width="350px"
                    onSelect={(value) => {
                      formik.setFieldValue('user_id', value);
                    }}
                    optionList={userData}
                    error={formik.touched.user_id && formik.errors.user_id}
                  />
                </div>
              </div>

              <div className={Styles.fields_container_2}>
                <div>
                  <DatePicker
                    label="Access Start Date"
                    name="access_start_date"
                    onChange={formik.handleChange}
                    width="350px"
                    value={formik.values.access_start_date}
                    error={
                      formik.touched.access_start_date &&
                      formik.errors.access_start_date
                    }
                  />
                </div>
                <div>
                  <DatePicker
                    label="Access Expiration Date"
                    name="access_end_date"
                    onChange={formik.handleChange}
                    width="350px"
                    value={formik.values.access_end_date}
                    // mandatory
                    error={
                      formik.touched.access_end_date &&
                      formik.errors.access_end_date
                    }
                  />
                </div>
                <div className={Styles.inputField}>
                  <div>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      icon={<AddIcon color="white" />}
                    >
                      Add To Project
                    </Button>
                  </div>
                </div>
              </div>
            </div>
          </form>
        </div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Existing Members *</h3>
            <span className={Styles.content}>
              Members of {props?.projectData?.data?.project_name}
            </span>
          </div>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <Input
                width="260px"
                prefixIcon={<SearchIcon />}
                name="global_search"
                value={filterValues.global_search}
                onChange={(e) => handleFilterChange(e)}
                placeholder="Search by Name and Role"
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
              <table className={Styles.scrollable_table}>
                <thead>
                  <tr>
                    <th>S NO</th>
                    <th>Name</th>
                    <th>Role</th>
                    <th>Expiration Date</th>
                    {activeButton === 'AC' && <th>Action</th>}
                  </tr>
                </thead>
                <tbody>
                  {dataShow ? (
                    getFilterData?.total_count === 0 ? (
                      <tr>
                        <td colSpan="4" style={{ textAlign: 'center' }}>
                          No data found
                        </td>
                        {activeButton === 'AC' && <td></td>}
                      </tr>
                    ) : (
                      getFilterData?.content?.map(
                        (data: any, index: number) => (
                          <tr key={data.project_member_association_id}>
                            <td>{startingIndex + index}</td>
                            <td>
                              <div className={Styles.profileDetail}>
                                <div>
                                  <Avatar
                                    firstName={data?.user_data?.first_name}
                                    lastName={data?.user_data?.last_name}
                                    size={40}
                                  />
                                </div>
                                <div className={Styles.profileContents}>
                                  <span className={Styles.profileName}>
                                    {data?.user_data?.first_name}{' '}
                                    {data?.user_data?.last_name}
                                  </span>
                                  <span className={Styles.emailContent}>
                                    {data?.user_data?.email_id}
                                  </span>
                                </div>
                              </div>
                            </td>
                            <td>{data?.project_role_data?.role_name}</td>
                            <td>
                              {data?.access_end_date == null
                                ? '-'
                                : format(
                                    new Date(data?.access_end_date),
                                    'MMM dd, yyyy'
                                  )}
                            </td>
                            {activeButton === 'AC' && (
                              <td>
                                <div className={Styles.tablerow}>
                                  <DeleteIcon
                                    onClick={() =>
                                      deleteProjectMember(
                                        data?.project_member_association_id
                                      )
                                    }
                                  />
                                </div>
                              </td>
                            )}
                          </tr>
                        )
                      )
                    )
                  ) : initialData?.total_count === 0 ? (
                    <tr>
                      <td colSpan="4" style={{ textAlign: 'center' }}>
                        No data found
                      </td>
                      {activeButton === 'AC' && <td></td>}
                    </tr>
                  ) : (
                    initialData?.content?.map((data: any, index: number) => (
                      <tr key={data.project_member_association_id}>
                        <td>{startingIndex + index}</td>
                        <td>
                          <div className={Styles.profileDetail}>
                            <div>
                              <Avatar
                                firstName={data?.user_data?.first_name}
                                lastName={data?.user_data?.last_name}
                                size={40}
                              />
                            </div>
                            <div className={Styles.profileContents}>
                              <span className={Styles.profileName}>
                                {data?.user_data?.first_name}{' '}
                                {data?.user_data?.last_name}
                              </span>
                              <span className={Styles.emailContent}>
                                {data?.user_data?.email_id}
                              </span>
                            </div>
                          </div>
                        </td>
                        <td>{data?.project_role_data?.role_name}</td>
                        <td>
                          {data?.access_end_date == null
                            ? '-'
                            : format(
                                new Date(data?.access_end_date),
                                'MMM dd, yyyy'
                              )}
                        </td>
                        {activeButton === 'AC' && (
                          <td>
                            <div className={Styles.tablerow}>
                              <DeleteIcon
                                onClick={() =>
                                  deleteProjectMember(
                                    data?.project_member_association_id
                                  )
                                }
                              />
                            </div>
                          </td>
                        )}
                      </tr>
                    ))
                  )}
                </tbody>
              </table>
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
      </CustomLoader>
      <CustomDelete
        open={openDelete}
        title="Delete Project Member"
        contentLine1="Are you sure you want to delete this Project Member ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteMember}
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default ProjectSettings;
