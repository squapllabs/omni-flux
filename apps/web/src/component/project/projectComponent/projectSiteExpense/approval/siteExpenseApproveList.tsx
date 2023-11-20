import React, { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import Styles from '../../../../../styles/expenseApprove.module.scss';
import { format } from 'date-fns';
import CustomLoader from '../../../../ui/customLoader';
import AutoCompleteSelect from '../../../../ui/AutoCompleteSelect';
import { store, RootState } from '../../../../../redux/store';
import { getToken } from '../../../../../redux/reducer';
import { useUpdatesiteExpense } from '../../../../../hooks/expense-hook';
import { useGetUserIDBasedProject } from '../../../../../hooks/project-hooks';
import ApproveDialogBox from '../../../../ui/CustomApprovePopup';
import siteExpenseService from '../../../../../service/expense-service';
import CustomSnackBar from '../../../../ui/customSnackBar';
import RejectDialogBox from '../../../../ui/CustomReject';
import { formatBudgetValue } from '../../../../../helper/common-function';
import CustomPagination from '../../../../menu/CustomPagination';
import ProjectSubheader from '../../../../project/projectSubheader';
import { useGetAllPaginatedExpense } from '../../../../../hooks/expense-hook';
import projectService from '../../../../../service/project-service';
import CustomGroupButton from '../../../../ui/CustomGroupButton';
import CustomMenu from '../../../../ui/CustomMenu';
import projectSettingsService from '../../../../../service/projectSettings-service';
import ViewIcon from '../../../../menu/icons/newViewIcon';

const ExpenseApprove = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const [siteData, setSiteData] = useState();
  const [projectMemberData, setProjectMemberData] = useState();
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
  const navigate = useNavigate();
  let rowIndex = 0;
  const { mutate: updateSiteExpenseData } = useUpdatesiteExpense();

  const { data: getProjectList = [], isLoading: dropLoading } =
    useGetUserIDBasedProject(roleName === 'ADMIN' ? '' : userID);

  const fetchProjectSiteData = async (value: any) => {
    if (value) {
      const getData = await projectService.getOneProjectSite(value);
      const arr: any = [];
      const siteList = getData?.data.map((site: any, index: any) => {
        const obj: any = {
          value: site?.site_id,
          label: site?.site_details?.name,
        };
        arr.push(obj);
      });
      setSiteData(arr);
    }
  };

  const fetchMemberData = async (value: any) => {
    if (value) {
      const getData = await projectSettingsService.fetchAllProjectMembers(
        value
      );
      const arr: any = [];
      const memberList = getData?.data?.map((user: any, index: any) => {
        const Obj: any = {
          value: user?.user_id,
          label: user?.user_data?.first_name + ' ' + user?.user_data?.last_name,
        };
        arr.push(Obj);
      });
      setProjectMemberData(arr);
    }
  };

  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [filterValue, setFilterValue] = useState<any>({});
  const [siteValue, setSiteValue] = useState<any>({});
  const [memberValue, setMemberValue] = useState<any>({});
  const [openApprove, setOpenApprove] = useState(false);
  const [openReject, setOpenReject] = useState(false);
  const [value, setValue] = useState();
  const [reload, setReload] = useState(false);
  const [expenseBill, setExpenseBill] = useState<any>([]);
  const [expenseList, setExpenseList] = useState<any>([]);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'All', value: 'All' },
    { label: 'Awaiting Approval', value: 'Pending' },
    { label: 'InProgress', value: 'InProgress' },
    { label: 'Completed', value: 'Completed' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('All');
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
    setCurrentPage(1);
  };

  const [initialValues, setInitialValues] = useState({
    employee_name: '',
    employee_id: '',
    employee_phone: '',
    end_date: '',
    start_date: '',
    purpose: '',
    department: '',
    designation: '',
    site_expense_id: '',
    site_id: '',
    expense_id: '',
    comments: '',
    status: '',
    progressed_by: '',
    updated_by: '',
  });

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };

  useEffect(() => {
    const fetchData = async () => {
      const datas = await siteExpenseService.getOnesiteExpenseByID(value);
      const arry: any = [];
      setExpenseBill(
        datas?.data?.bill_details === null ? [] : datas?.data?.bill_details
      );
      setExpenseList(datas?.data?.expense_details);
      setInitialValues({
        employee_name: datas?.data?.employee_name,
        employee_id: datas?.data?.employee_id,
        employee_phone: datas?.data?.employee_phone,
        end_date: dateFormat(datas?.data?.end_date),
        start_date: dateFormat(datas?.data?.start_date),
        purpose: datas?.data?.purpose,
        department: datas?.data?.department,
        designation: datas?.data?.designation,
        site_expense_id: datas?.data?.site_expense_id,
        expense_id: datas?.data?.expense_id,
        site_id: datas?.data?.site_id,
        comments: datas?.data?.comments,
        status: datas?.data?.status,
        progressed_by: datas?.data?.progressed_by,
        updated_by: datas?.data?.updated_by,
      });
    };
    if (value !== undefined) fetchData();
  }, [reload, value]);

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

  const handleCloseApprove = () => {
    setOpenApprove(false);
  };

  const handleCloseReject = () => {
    setOpenReject(false);
  };

  const approveHandler = (id: any) => {
    setValue(id);
    setOpenApprove(true);
  };

  const rejectHandler = (id: any) => {
    setValue(id);
    setOpenReject(true);
  };

  const approveSite = async () => {
    const object: any = {
      site_id: initialValues.site_id,
      employee_name: initialValues.employee_name,
      employee_id: initialValues.employee_id,
      employee_phone: initialValues.employee_phone,
      end_date: initialValues.end_date,
      start_date: initialValues.start_date,
      purpose: initialValues.purpose,
      department: initialValues.department,
      designation: initialValues.designation,
      expense_details: expenseList,
      updated_by: encryptedData?.userId,
      expense_id: initialValues.expense_id,
      bill_details: expenseBill,
      status: 'Approved',
      progressed_by: userID,
    };
    updateSiteExpenseData(object, {
      onSuccess(data, variables, context) {
        if (data?.status === true) {
          setMessage('Site Expense has been Approved');
          setOpenSnack(true);
          setReload(true);
          handleCloseApprove();
        }
      },
    });
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleRejectWithComments = (comments: string) => {
    const object: any = {
      site_id: initialValues.site_id,
      employee_name: initialValues.employee_name,
      employee_id: initialValues.employee_id,
      employee_phone: initialValues.employee_phone,
      end_date: initialValues.end_date,
      start_date: initialValues.start_date,
      purpose: initialValues.purpose,
      department: initialValues.department,
      designation: initialValues.designation,
      expense_details: expenseList,
      updated_by: encryptedData?.userId,
      expense_id: initialValues.expense_id,
      bill_details: expenseBill,
      comments: comments,
      status: 'Rejected',
      progressed_by: userID,
    };
    updateSiteExpenseData(object, {
      onSuccess(data, variables, context) {
        if (data?.status === true) {
          setMessage('Site Expense has been Rejected');
          setOpenSnack(true);
          setReload(true);
          handleCloseReject();
        }
      },
    });
  };

  const demo: any = {
    offset: (currentPage - 1) * rowsPerPage,
    limit: rowsPerPage,
    order_by_column: 'updated_date',
    order_by_direction: 'desc',
    status: 'AC',
    user_id: roleName === 'ADMIN' ? null : userID,
    expense_status: activeButton,
    ...filterValue,
    site_id: siteValue.site_id,
    employee_name: memberValue.project_member_name,
    is_draft: 'N',
  };

  const {
    isLoading: getAllLoadingPaginated,
    data: initialData,
    refetch,
  } = useGetAllPaginatedExpense(demo);

  useEffect(() => {
    refetch();
  }, [currentPage, rowsPerPage, activeButton]);

  useEffect(() => {
    const handleSearch = setTimeout(() => {
      refetch();
    }, 500);
    return () => clearTimeout(handleSearch);
  }, [filterValue, siteValue, memberValue]);

  return (
    <div className={Styles.container}>
      <CustomLoader loading={getAllLoadingPaginated} size={48} color="#333C44">
        <div className={Styles.box}>
          <ProjectSubheader
            navigation={'/home'}
            title="Claim Approval for Site"
            description="Review and authorization of expenses incurred at a designated site"
          />
        </div>
        <div className={Styles.tableContainer}>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div className={Styles.filterSelect}>
                <AutoCompleteSelect
                  name="project_id"
                  label="Project"
                  placeholder="Select Project"
                  optionList={dropLoading === true ? [] : getProjectList}
                  value={filterValue.project_id}
                  onSelect={(value) => {
                    setFilterValue({ 'project_id': value });
                    fetchProjectSiteData(value);
                    fetchMemberData(value);
                    setCurrentPage(1);
                  }}
                  width="90%"
                />
              </div>
              <div className={Styles.selectRow}>
                <div className={Styles.selectDrop}>
                  <AutoCompleteSelect
                    name="site_id"
                    label="Site"
                    placeholder="Select Site"
                    optionList={siteData}
                    onSelect={(value) => {
                      setSiteValue({ site_id: value });
                    }}
                    width="200px"
                  />
                </div>
                <div className={Styles.selectDrop}>
                  <AutoCompleteSelect
                    name="project_member_id"
                    label="Project Member"
                    placeholder="Select Project Member"
                    optionList={projectMemberData}
                    onSelect={(value) => {
                      setMemberValue({ project_member_id: value });
                      const matchingObjects = projectMemberData.filter(
                        (obj: any) => Number(obj.value) === Number(value)
                      );
                      setMemberValue({
                        project_member_name: matchingObjects[0].label,
                      });
                    }}
                    width="200px"
                  />
                </div>
              </div>
            </div>
          </div>
          <div className={Styles.button}>
            <CustomGroupButton
              labels={buttonLabels}
              onClick={handleGroupButtonClick}
              activeButton={activeButton}
            />
            <div>
              {activeButton === 'Completed' ? (
                <span className={Styles.message}>
                  {' '}
                  * denotes that expense has been recalled
                </span>
              ) : (
                ''
              )}
            </div>
          </div>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>#</th>
                <th>Expense Code</th>
                <th>Project</th>
                <th>Site</th>
                <th>Added By</th>
                <th>Amount</th>
                <th>Status</th>
                <th>Action</th>
              </tr>
            </thead>
            <tbody className="globaltablebody">
              {initialData?.content?.length === 0 ? (
                <tr>
                  <td colSpan="7" style={{ textAlign: 'center' }}>
                    No data found
                  </td>
                </tr>
              ) : (
                ''
              )}
              {initialData?.content?.map((items: any, index: any) => {
                if (items.is_delete !== true) {
                  rowIndex = rowIndex + 1;
                  const sumOfRates = items?.expense_details.reduce(
                    (accumulator: any, currentItem: any) => {
                      return accumulator + currentItem.total;
                    },
                    0
                  );
                  let is_recall = false;
                  items?.expense_details.map((data: any) => {
                    if (data?.is_recalled === true) {
                      is_recall = true;
                    }
                  });
                  return (
                    <tr key={index}>
                      <td>{rowIndex}</td>
                      <td>{items?.expense_code}</td>
                      <td>{items?.project_data?.project_name || "-"}</td>
                      <td>{items?.site_data?.name || "-"}</td>
                      <td>{items?.employee_name}</td>
                      <td>{formatBudgetValue(sumOfRates)}</td>
                      {is_recall &&
                      items?.status === 'Completed' &&
                      activeButton === 'Completed' ? (
                        <td style={{ display: 'flex' }}>
                          <span className={Styles.approvedStatus}>
                            {items?.status}
                          </span>
                          <span className={Styles.symbol}>*</span>
                        </td>
                      ) : !is_recall && items?.status === 'Completed' ? (
                        <td>
                          <span className={Styles.approvedStatus}>
                            {items?.status}
                          </span>
                        </td>
                      ) : (
                        <td>
                          <span
                            className={`${Styles.status} ${
                              items?.status === 'Pending'
                                ? Styles.pendingStatus
                                : items?.status === 'InProgress'
                                ? Styles.rejectedStatus
                                : items?.status === 'Draft'
                                ? Styles.draftStatus
                                : items?.status === 'Completed'
                                ? Styles.approvedStatus
                                : ''
                            }`}
                          >
                            {items?.status === 'Pending'
                              ? 'Waiting for Approval'
                              : items?.status}
                          </span>
                        </td>
                      )}
                      <td>
                        <ViewIcon
                          onClick={() =>
                            navigate(
                              `/expense-detail-approve/${items?.project_data?.project_id}/${items.expense_id}`
                            )
                          }
                        />
                      </td>
                    </tr>
                  );
                }
              })}
            </tbody>
          </table>
          <div className={Styles.pagination}>
            <CustomPagination
              currentPage={currentPage}
              totalPages={initialData?.total_page}
              totalCount={initialData?.total_count}
              rowsPerPage={rowsPerPage}
              onPageChange={handlePageChange}
              onRowsPerPageChange={handleRowsPerPageChange}
            />
          </div>
        </div>
      </CustomLoader>
      <ApproveDialogBox
        open={openApprove}
        title="Approve Site Expense"
        contentLine1=""
        contentLine2=""
        handleClose={handleCloseApprove}
        handleConfirm={approveSite}
      />
      <RejectDialogBox
        open={openReject}
        title="Reject Site Expense"
        contentLine1="Are you sure want to reject this expense ?"
        contentLine2=""
        handleClose={handleCloseReject}
        onReject={handleRejectWithComments}
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
export default ExpenseApprove;
