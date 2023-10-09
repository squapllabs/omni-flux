import React, { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import Styles from '../../../../../styles/expenseApprove.module.scss';
import { format } from 'date-fns';
import CustomLoader from '../../../../ui/customLoader';
import AutoCompleteSelect from '../../../../ui/AutoCompleteSelect';
import { store, RootState } from '../../../../../redux/store';
import { getToken } from '../../../../../redux/reducer';
import { updatesiteExpense } from '../../../../../hooks/expense-hook';
import { getUserIDProjectRolebased } from '../../../../../hooks/project-hooks';
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

const ExpenseApprove = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const [siteData, setSiteData] = useState();
  const [projectMemberData, setProjectMemberData] = useState();
  const roleID: number = encryptedData.userData.user_roles[0].role_data.role_id;
  const navigate = useNavigate();
  let rowIndex = 0;
  const { mutate: updateSiteExpenseData } = updatesiteExpense();
  const Obj: any = {
    userID: Number(userID),
    roleID: Number(roleID),
  };
  const { data: getProjectList = [], isLoading: dropLoading } =
    getUserIDProjectRolebased(Obj);
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
      const memberList = getData?.data?.map((user:any,index:any) =>{
        const Obj: any = {
          value: user?.user_id,
          label: user?.user_data?.first_name +' '+ user?.user_data?.last_name,
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
    { label: 'All', value: '' },
    { label: 'Pending', value: 'Pending' },
    { label: 'Approved', value: 'Approved' },
    { label: 'Rejected', value: 'Rejected' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('Pending');
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
    user_id: userID,
    expense_status: activeButton,
    ...filterValue,
    site_id: siteValue.site_id,
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
  }, [filterValue, siteValue]);

  return (
    <div className={Styles.container}>
      <CustomLoader loading={getAllLoadingPaginated} size={48} color="#333C44">
        <div className={Styles.box}>
          <ProjectSubheader
            navigation={'/home'}
            title="Expense Approval for Site"
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
                  optionList={dropLoading === true ? [] : getProjectList}
                  value={filterValue.project_id}
                  onSelect={(value) => {
                    setFilterValue({ ['project_id']: value });
                    fetchProjectSiteData(value);
                    fetchMemberData(value);
                  }}
                  width="100%"
                />
              </div>
              <div>
                <AutoCompleteSelect
                  name="site_id"
                  label="Site"
                  optionList={siteData}
                  onSelect={(value) => {
                    setSiteValue({ ['site_id']: value });
                  }}
                  width="100%"
                />
              </div>
              <div>
                <AutoCompleteSelect
                  name="site_engineer_id"
                  label="Site Engineer Name"
                  optionList={projectMemberData}
                  onSelect={(value) => {
                    setMemberValue({ ['project_member_id']: value });
                    const matchingObjects = projectMemberData.filter(
                      (obj: any) => Number(obj.value) === Number(value)
                    );
                    setMemberValue({ ['project_member_name']: matchingObjects[0].label });
                  }}
                  width="100%"
                />
              </div>
            </div>
            <div className={Styles.button}>
              <CustomGroupButton
                labels={buttonLabels}
                onClick={handleGroupButtonClick}
                activeButton={activeButton}
              />
            </div>
          </div>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>#</th>
                <th className={Styles.tableHeading}>Expense Code</th>
                <th className={Styles.tableHeading}>Project</th>
                <th className={Styles.tableHeading}>Site</th>
                <th className={Styles.tableHeading}>Added By</th>
                <th className={Styles.tableHeading}>From Date</th>
                <th className={Styles.tableHeading}>To Date</th>
                <th className={Styles.tableHeading}>Amount</th>
                <th className={Styles.tableHeading}>Status</th>
                {activeButton === 'Pending' && (
                  <th className={Styles.tableHeading}>Action</th>
                )}
              </tr>
            </thead>
            <tbody>
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
                  const actions = [
                    {
                      label: 'Info',
                      onClick: () => {
                        navigate(
                          `/expense-detail-approve/${items?.project_data?.project_id}/${items.expense_id}`
                        );
                      },
                    },
                    {
                      label: 'Approve',
                      onClick: () => {
                        approveHandler(items.expense_id);
                      },
                    },
                    {
                      label: 'Reject',
                      onClick: () => {
                        rejectHandler(items.expense_id);
                      },
                    },
                  ];
                  return (
                    <tr>
                      <td>{rowIndex}</td>
                      <td>{items?.expense_code}</td>
                      <td>{items?.project_data?.project_name}</td>
                      <td>{items?.site_data?.name}</td>
                      <td>{items?.employee_name}</td>
                      <td>{dateFormat(items?.start_date)}</td>
                      <td>{dateFormat(items?.end_date)}</td>
                      <td>{formatBudgetValue(sumOfRates)}</td>
                      <td>{items?.status}</td>
                      {activeButton === 'Pending' && (
                        <td>
                          <CustomMenu actions={actions} />
                        </td>
                      )}
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
