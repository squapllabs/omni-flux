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
import { useGetAllSelectedRoles } from '../../../hooks/userRole-hooks';
import {
  createProjectMember,
  getBySearchProjectMembers,
  useGetAllPaginatedProjectMember,
  useDeleteProjectMember,
} from '../../../hooks/projectSettings-hook';
import ProjectSettingsService from '../../../service/projectSettings-service';
import { format } from 'date-fns';
import Avatar from '../../menu/AvatarComponent';
import DeleteIcon from '../../menu/icons/newDeleteIcon';
import CustomLoader from '../../ui/customLoader';
import Pagination from '../../menu/CustomPagination';
import CustomDelete from '../../ui/customDeleteDialogBox';
import CustomSnackBar from '../../ui/customSnackBar';
import * as Yup from 'yup';
import {
  getProjectMemberCreationYupschema,
  getCreateMasterValidateyup,
} from '../../../helper/constants/projectSettings-constants';
import TextArea from '../../ui/CustomTextArea';
import {
  useGetAllmasertData,
  createmasertData,
  useGetAllPaginatedMasterData,
  useDeletemasertData,
} from '../../../hooks/masertData-hook';
import { getByProjectId } from '../../../hooks/project-hooks';
import EditIcon from '../../menu/icons/editIcon';
import CustomEditDialog from '../../ui/customEditDialogBox';
import ProjectMasterDataEditForm from './projectMasterDataEdit';
import MemberIcon from '../../menu/icons/memberIcon';
import CustomProjectMemberAddPopup from './projectMemberAddPopup';
import CustomPopup from '../../ui/CustomSidePopup';

const ProjectSettings: React.FC = (props: any) => {
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [isResetDisabled, setIsResetDisabled] = useState(true);
  const [open, setOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [reload, setReload] = useState(false);
  const [message, setMessage] = useState('');
  const [isLoading, setIsLoading] = useState(true);
  const [dataShow, setDataShow] = useState(false);
  const [componentShow, setComponentShow] = useState(false);
  const [filterValues, setFilterValues] = useState({
    global_search: '',
  });
  const { mutate: createNewProjectMember } = createProjectMember();
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const routeParams = useParams();
  const [userData, setUserData] = useState();
  const { data: getAllRolesData = [], isLoading: dropLoading } =
    useGetAllSelectedRoles();
  const { mutate: getDeleteProjectMemberByID } = useDeleteProjectMember();

  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [filter, setFilter] = useState(false);
  const [projectId, setProjectId] = useState(Number(routeParams?.id));

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

  const handleClosePopup = () => {
    setOpen(false);
  }

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
  // const handleSearch = async () => {
  //   const demo: any = {
  //     offset: (currentPage - 1) * rowsPerPage,
  //     limit: rowsPerPage,
  //     order_by_column: 'updated_date',
  //     order_by_direction: 'desc',
  //     status: activeButton,
  //     project_id: Number(routeParams?.id),
  //     ...filterValues,
  //   };
  //   postDataForFilter(demo);
  //   setDataShow(true);
  //   setIsLoading(false);
  //   setFilter(true);
  // };

  /* Function for resting the search field and data to normal state */
  // const handleReset = async () => {
  //   const demo: any = {
  //     offset: (currentPage - 1) * rowsPerPage,
  //     limit: rowsPerPage,
  //     order_by_column: 'updated_date',
  //     order_by_direction: 'desc',
  //     status: 'AC',
  //     project_id: Number(routeParams?.id),
  //     global_search: '',
  //   };
  //   postDataForFilter(demo);
  //   setIsLoading(false);
  //   setFilter(false);
  //   setFilterValues({
  //     global_search: '',
  //   });
  //   setIsLoading(false);
  //   setDataShow(false);
  //   setIsResetDisabled(true);
  // };

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
    refetch();
    setActiveButton(activeButton);
  };

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.conatiner}>
      <CustomLoader
        loading={FilterLoading ? FilterLoading : getAllLoadingProjectMemberData}
        size={48}
        color="#333C44"
      >
        {initialData?.total_count !== 0 || activeButton === 'IN' ? (
          <div>
            <div className={Styles.topHeading}>
              <div className={Styles.heading}>
                <div className={Styles.subHeading}>
                  <MemberIcon />
                  <h3>MEMBERS</h3>
                </div>
                <div>
                  <Button
                    color="primary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    icon={<AddIcon color="white" />}
                    onClick={() => setOpen(true)}
                  >
                    Add Member
                  </Button>
                </div>
              </div>
              <div>
                <CustomGroupButton
                  labels={buttonLabels}
                  onClick={handleGroupButtonClick}
                  activeButton={activeButton}
                />
              </div>
            </div>
            <div className={Styles.box}>
              <div className={Styles.tableContainer}>
                <div>
                  <table className={Styles.scrollable_table}>
                    <thead>
                      <tr>
                        <th>#</th>
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
                <div>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={
                      dataShow
                        ? getFilterData?.total_page
                        : initialData?.total_page
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
          </div>
        ) : (
          <div>
            <div className={Styles.subHeading}>
              <MemberIcon />
              <h3>MEMBERS</h3>
            </div>
            <div className={Styles.emptyDataHandling}>
              <div className={Styles.imageAdd}>
                <img
                  src="/add-member.png"
                  alt="aa"
                  width="80%"
                  height="20%"
                />
              </div>
              <div>
                <h5 className={Styles.textmax}>This project has no members.</h5>
              </div>
              <div>
                <p className={Styles.textmin}>Go ahead, add a member to this project now</p>
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  icon={<AddIcon color="white" />}
                  onClick={() => setOpen(true)}
                >
                  Add Member
                </Button>
              </div>
            </div>
          </div>

        )}
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
      <CustomPopup
        title="Add Member"
        open={open}
        handleClose={handleClosePopup}
        content={
          <CustomProjectMemberAddPopup
            setOpen={setOpen}
            open={open}
            setReload={setReload}
            setOpenSnack={setOpenSnack}
            setMessage={setMessage}
            projectId={projectId}
          />
        }
      />
    </div>
  );
};

export default ProjectSettings;

