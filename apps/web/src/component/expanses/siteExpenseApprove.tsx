import React, { useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import Button from '../ui/Button';
import Styles from '../../styles/expenseApprove.module.scss';
import Pagination from '../menu/pagination';
import { format } from 'date-fns';
import CustomLoader from '../ui/customLoader';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import {
  getBySearchsiteExpense,
  updatesiteExpense,
} from '../../hooks/expense-hook';
import { getUserIDProjectRolebased } from '../../hooks/project-hooks';
import InfoIcon from '../menu/icons/infoIcon';
import TickIcon from '../menu/icons/tickIcon';
import ApproveDialogBox from '../ui/CustomApprovePopup';
import siteExpenseService from '../../service/expense-service';
import CustomSnackBar from '../ui/customSnackBar';
import RejectIcon from '../menu/icons/cancelIcon';
import RejectDialogBox from '../ui/CustomReject';
import { formatBudgetValue } from '../../helper/common-function';
import CustomPagination from '../menu/CustomPagination';
import ProjectSubheader from '../project/projectSubheader';

const ExpenseApprove = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID: number = encryptedData.userId;
  const roleID: number = encryptedData.userData.user_roles[0].role_data.role_id;
  const navigate = useNavigate();
  let rowIndex = 0;
  const {
    mutate: postDataForFilter,
    data: getExpenseList,
    isLoading: fetchLoader,
  } = getBySearchsiteExpense();

  const { mutate: updateSiteExpenseData } = updatesiteExpense();

  const Obj: any = {
    userID: Number(userID),
    roleID: Number(roleID),
  };

  const { data: getProjectList = [] } = getUserIDProjectRolebased(Obj);
  const [currentPage, setCurrentPage] = useState(1);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [filterValue, setFilterValue] = useState<any>({});
  const [openApprove, setOpenApprove] = useState(false);
  const [openReject, setOpenReject] = useState(false);
  const [value, setValue] = useState();
  const [reload, setReload] = useState(false);
  const [expenseBill, setExpenseBill] = useState<any>([]);
  const [expenseList, setExpenseList] = useState<any>([]);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [comments, setComments] = useState('');

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

  const handleSearch = async () => {
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      user_id: userID,
      expense_status: 'Pending',
      ...filterValue,
    };
    postDataForFilter(demo);
  };

  const handleReset = async () => {
    setFilterValue('');
    const demo: any = {
      offset: (currentPage - 1) * rowsPerPage,
      limit: rowsPerPage,
      order_by_column: 'updated_date',
      order_by_direction: 'desc',
      status: 'AC',
      global_search: '',
      user_id: userID,
      expense_status: 'Pending',
    };
    postDataForFilter(demo);
  };

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };

  useEffect(() => {
    handleSearch();
  }, [currentPage, rowsPerPage]);

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
          handleReset();
          setTimeout(() => {
            navigate('/settings');
          }, 3000);
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
          handleReset();
          setTimeout(() => {
            navigate('/settings');
          }, 3000);
        }
      },
    });
  };

  return (
    <div className={Styles.container}>
      <CustomLoader loading={fetchLoader}>
        <div className={Styles.box}>
          <ProjectSubheader navigation={'/home'} title="Expense Approval for Site" description="Review and authorization of expenses incurred at a designated site"/>
        </div>
        <div className={Styles.tableContainer}>
          <div className={Styles.searchField}>
            <div className={Styles.inputFilter}>
              <div className={Styles.filterSelect}>
                <AutoCompleteSelect
                  name="project_id"
                  label="Project"
                  optionList={getProjectList}
                  value={filterValue.project_id}
                  onSelect={(value) => {
                    setFilterValue({ ...filterValue, ['project_id']: value });
                  }}
                />
              </div>
              <div className={Styles.filterButton}>
                <Button
                  className={Styles.searchButton}
                  type="button"
                  color="primary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  onClick={() => handleSearch()}
                >
                  Search
                </Button>
                <Button
                  className={Styles.resetButton}
                  type="button"
                  color="secondary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  onClick={() => handleReset()}
                >
                  Reset
                </Button>
              </div>
            </div>
          </div>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th className={Styles.tableHeading}>#</th>
                <th className={Styles.tableHeading}>Expense Code</th>
                <th className={Styles.tableHeading}>Project</th>
                <th className={Styles.tableHeading}>Site</th>
                <th className={Styles.tableHeading}>From Date</th>
                <th className={Styles.tableHeading}>To Date</th>
                <th className={Styles.tableHeading}>Amount</th>
                <th className={Styles.tableHeading}>Status</th>
                <th className={Styles.tableHeading}>Action</th>
              </tr>
            </thead>
            <tbody>
              {getExpenseList?.content?.length === 0 ? (
                <tr>
                  <td colSpan="7" style={{ textAlign: 'center' }}>
                    No data found
                  </td>
                </tr>
              ) : (
                ''
              )}
              {getExpenseList?.content?.map((items: any, index: any) => {
                if (items.is_delete !== true) {
                  rowIndex = rowIndex + 1;
                  const sumOfRates = items?.expense_details.reduce(
                    (accumulator: any, currentItem: any) => {
                      return accumulator + currentItem.total;
                    },
                    0
                  );
                  return (
                    <tr>
                      <td>{rowIndex}</td>
                      <td>{items?.expense_code}</td>
                      <td>{items?.project_data?.project_name}</td>
                      <td>{items?.site_data?.name}</td>
                      <td>{dateFormat(items?.start_date)}</td>
                      <td>{dateFormat(items?.end_date)}</td>
                      <td>{formatBudgetValue(sumOfRates)}</td>
                      <td>{items?.status}</td>
                      <td>
                        <div className={Styles.tableIcon}>
                          <InfoIcon
                            onClick={() =>
                              navigate(
                                `/expense-detail-approve/${items?.project_data?.project_id}/${items.expense_id}`
                              )
                            }
                          />
                          <TickIcon
                            onClick={() => approveHandler(items.expense_id)}
                          />
                          <RejectIcon
                            onClick={() => rejectHandler(items.expense_id)}
                          />
                        </div>
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
              totalPages={getExpenseList?.total_page}
              totalCount={getExpenseList?.total_count}
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
