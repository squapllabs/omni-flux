import React, { useEffect, useState } from 'react';
import { store, RootState } from '../../../../../redux/store';
import { getToken } from '../../../../../redux/reducer';
import siteExpenseService from '../../../../../service/expense-service';
import { format } from 'date-fns';
import CustomSnackBar from '../../../../ui/customSnackBar';
import { useParams, useNavigate } from 'react-router-dom';
import CustomCard from '../../../../ui/CustomCard';
import Styles from '../../../../../styles/expenseApprove.module.scss';
import ApproveDialogBox from '../../../../ui/CustomApprovePopup';
import RejectDialogBox from '../../../../ui/CustomReject';
import { updatesiteExpenseDetail } from '../../../../../hooks/expense-hook';
import { environment } from '../../../../../environment/environment';
import ProjectSubheader from '../../../../project/projectSubheader';
import CustomMenu from '../../../../ui/CustomMenu';
import { formatBudgetValue } from '../../../../../helper/common-function';

const ExpenseDetailApprove = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID = encryptedData.userId;
  const params = useParams();
  const navigate = useNavigate();
  const projectId = Number(params?.projectId);
  const expenseId = Number(params?.id);
  const [tableData, setTableData] = useState<any>([]);
  const [value, setValue] = useState(0);
  const [openApprove, setOpenApprove] = useState(false);
  const [openReject, setOpenReject] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [reload, setReload] = useState(false);
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;
  const [initialValues, setInitialValues] = useState({
    expense_details_id: '',
    status: '',
    comments: '',
    created_by: '',
    progressed_by: '',
    updated_by: '',
  });

  const { mutate: updateSiteExpenseDetailData } = updatesiteExpenseDetail();

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  let rowindex = 0;

  useEffect(() => {
    const fetchData = async () => {
      const datas = await siteExpenseService.getOnesiteExpenseByID(params?.id);
      setTableData(datas.data);
    };
    if (expenseId !== undefined) fetchData();
    setReload(false);
  }, [reload]);

  useEffect(() => {
    const fetchData = async () => {
      const data = await siteExpenseService.getOnesiteExpenseDetailByID(value);
      setInitialValues({
        expense_details_id: data?.data?.expense_details_id,
        status: data?.data?.status,
        comments: data?.data?.comments,
        created_by: data?.data?.created_by,
        progressed_by: userID,
        updated_by: '',
      });
    };
    if (value !== undefined) fetchData();
  }, [reload, value]);

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const approveHandler = (id: any) => {
    setValue(id);
    setOpenApprove(true);
  };

  const handleCloseApprove = () => {
    setOpenApprove(false);
  };

  const approveExpense = () => {
    const object: any = {
      expense_details_id: initialValues.expense_details_id,
      status: 'Approved',
      updated_by: userID,
      progressed_by: userID,
    };
    updateSiteExpenseDetailData(object, {
      onSuccess(data, variables, context) {
        if (data?.status === true) {
          setMessage('Site Expense Detail has been Approved');
          setOpenSnack(true);
          setReload(true);
          handleCloseApprove();
        }
      },
    });
  };

  const rejectHandler = (id: any) => {
    setValue(id);
    setOpenReject(true);
  };

  const handleCloseReject = () => {
    setOpenReject(false);
  };

  const handleRejectWithComments = (comments: string) => {
    const object: any = {
      expense_details_id: initialValues.expense_details_id,
      status: 'Rejected',
      comments: comments,
      updated_by: userID,
      progressed_by: userID,
    };
    setReload(false);
    updateSiteExpenseDetailData(object, {
      onSuccess(data, variables, context) {
        if (data?.status === true) {
          setMessage('Site Expense Detail has been Rejected');
          setOpenSnack(true);
          setReload(true);
          handleCloseReject();
        }
      },
    });
  };

  return (
    <div>
      <div>
        <ProjectSubheader
          title="Expense Details"
          navigation={'/site-expense-approve'}
          description=""
        />
      </div>
      <div className={Styles.cardContent}>
        <CustomCard>
          <div className={Styles.mainContent}>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Project Name</div>
              <div className={Styles.rightData}>
                {' '}
                {tableData?.project_data?.project_name
                  ? `${tableData?.project_data?.project_name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Site Name</div>
              <div className={Styles.rightData}>
                {' '}
                {tableData?.site_data?.name
                  ? `${tableData?.site_data?.name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Expense Code</div>
              <div className={Styles.rightData}>
                {' '}
                {tableData?.expense_code
                  ? `${tableData?.expense_code}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Employee ID</div>
              <div className={Styles.rightData}>
                {' '}
                {tableData?.employee_id
                  ? `${tableData?.employee_id}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Employee Name</div>
              <div className={Styles.rightData}>
                {' '}
                {tableData?.employee_name
                  ? `${tableData?.employee_name}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Start Date</div>
              <div className={Styles.rightData}>
                {' '}
                {tableData?.start_date
                  ? `${dateFormat(tableData?.start_date)}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>End Date</div>
              <div className={Styles.rightData}>
                {' '}
                {tableData?.end_date
                  ? `${dateFormat(tableData?.end_date)}`
                  : 'Not Provided'}
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.dataRows}>
              <div className={Styles.leftData}>Uploaded Documents</div>
              <div className={Styles.rightData}>
                <ol className={Styles.siteList}>
                  {tableData?.bill_details?.map((files: any, index: any) => (
                    <ol key={index}>
                      <a href={files.path}>Document {index + 1}</a>
                    </ol>
                  ))}
                </ol>
              </div>
            </div>
          </div>
        </CustomCard>
      </div>
      <div className={Styles.tableContainerBottom}>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th className={Styles.tableHeading}>#</th>
              <th className={Styles.tableHeading}>Description</th>
              <th className={Styles.tableHeading}>Expense Name</th>
              <th className={Styles.tableHeading}>Amount</th>
              <th className={Styles.tableHeading}>Documents</th>
              <th className={Styles.tableHeading}>Status</th>
              <th className={Styles.tableHeading}>Action</th>
            </tr>
          </thead>
          <tbody>
            {tableData?.expense_details?.length === 0 ? (
              <tr>
                <td colSpan="4" style={{ textAlign: 'center' }}>
                  No document found
                </td>
              </tr>
            ) : (
              ''
            )}
            {tableData?.expense_details?.map((data: any, index: any) => {
              if (data?.is_delete === false) {
                rowindex = rowindex + 1;
                const isApproved =
                  data?.status === 'Approved' || data?.status === 'Rejected';
                const actions = [
                  {
                    label: 'Approve',
                    onClick: () => {
                      approveHandler(data.expense_details_id);
                    },
                    disabled: isApproved,
                  },
                  {
                    label: 'Reject',
                    onClick: () => {
                      rejectHandler(data.expense_details_id);
                    },
                    disabled: isApproved,
                  },
                ];
                return (
                  <tr>
                    <td>{rowindex}</td>
                    <td>{data?.description || nullLableNameFromEnv}</td>
                    <td>
                      {data?.expense_master_data?.master_data_name ||
                        nullLableNameFromEnv}
                    </td>
                    <td>{formatBudgetValue(data?.total)}</td>
                    <td>
                      {data?.bill_details.length > 0
                        ? data?.bill_details?.map((files: any, index: any) => (
                            <ol key={index}>
                              <a href={files.path}>Document {index + 1}</a>
                            </ol>
                          ))
                        : nullLableNameFromEnv}
                    </td>
                    <td>
                      <span
                        className={`${Styles.status} ${
                          data?.status === 'Rejected'
                            ? Styles.rejectedStatus
                            : data?.status === 'Approved'
                            ? Styles.approvedStatus
                            : ''
                        }`}
                      >
                        {data?.status || nullLableNameFromEnv}
                      </span>
                    </td>
                    <td>
                      <CustomMenu actions={actions} />
                    </td>
                  </tr>
                );
              }
            })}
          </tbody>
        </table>
      </div>
      <ApproveDialogBox
        open={openApprove}
        title="Approve Site Expense"
        contentLine1=""
        contentLine2=""
        handleClose={handleCloseApprove}
        handleConfirm={approveExpense}
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
export default ExpenseDetailApprove;
