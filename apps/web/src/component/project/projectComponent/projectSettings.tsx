import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/projectSettings.module.scss';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import CustomGroupButton from '../../ui/CustomGroupButton';
import { useParams } from 'react-router-dom';
import {
  useGetAllPaginatedProjectMember,
  useDeleteProjectMember,
} from '../../../hooks/projectSettings-hook';
import { format } from 'date-fns';
import Avatar from '../../menu/AvatarComponent';
import DeleteIcon from '../../menu/icons/newDeleteIcon';
import CustomLoader from '../../ui/customLoader';
import Pagination from '../../menu/CustomPagination';
import CustomDelete from '../../ui/customDeleteDialogBox';
import CustomSnackBar from '../../ui/customSnackBar';
import MemberIcon from '../../menu/icons/memberIcon';
import CustomProjectMemberAddPopup from './projectMemberAddPopup';
import CustomPopup from '../../ui/CustomSidePopup';

const ProjectSettings: React.FC = (props: any) => {
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [open, setOpen] = useState(false);
  const [modalOpen, setModalOpen] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [value, setValue] = useState();
  const [message, setMessage] = useState('');
  const [filterValues, setFilterValues] = useState({
    global_search: '',
  });
  const [activeButton, setActiveButton] = useState<string | null>('AC');
  const routeParams = useParams();
  const { mutate: getDeleteProjectMemberByID } = useDeleteProjectMember();
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'Active', value: 'AC' },
    { label: 'Inactive', value: 'IN' },
  ]);
  const [projectId, setProjectId] = useState(Number(routeParams?.id));
  const object: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: activeButton,
    project_id: Number(routeParams?.id),
    global_search: filterValues.global_search,
  };
  /* Function to get all members related to project */
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
    setModalOpen(false);
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
  /* Function for group button (Active and Inactive status) */
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
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
  useEffect(() => {
    if (modalOpen === true) {
      document.body.style.overflow = 'hidden';
    } else {
      document.body.style.overflow = 'auto';
    }
  }, [modalOpen]);

  const startingIndex = (currentPage - 1) * rowsPerPage + 1;

  return (
    <div className={Styles.conatiner}>
      <CustomLoader
        loading={getAllLoadingProjectMemberData}
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
                    onClick={() => {
                      setOpen(true);
                      setModalOpen(true);
                    }}
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
                      {initialData?.total_count === 0 ? (
                        <tr>
                          <td colSpan="4" style={{ textAlign: 'center' }}>
                            No data found
                          </td>
                          {activeButton === 'AC' && <td></td>}
                        </tr>
                      ) : (
                        initialData?.content?.map(
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
                      )}
                    </tbody>
                  </table>
                </div>
                <div>
                  <Pagination
                    currentPage={currentPage}
                    totalPages={initialData?.total_page}
                    totalCount={initialData?.total_count}
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
                <img src="/add-member.png" alt="aa" width="80%" height="20%" />
              </div>
              <div>
                <h5 className={Styles.textmax}>This project has no members.</h5>
              </div>
              <div>
                <p className={Styles.textmin}>
                  Go ahead, add a member to this project now
                </p>
              </div>
              <div className={Styles.emptyButton}>
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
