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
import CustomConfirm from '../../../../ui/CustomConfirmDialogBox';
import {
  updatesiteExpenseStatus,
  updatesiteExpenseDetail,
} from '../../../../../hooks/expense-hook';
import { environment } from '../../../../../environment/environment';
import ProjectSubheader from '../../../../project/projectSubheader';
import CustomMenu from '../../../../ui/CustomMenu';
import { formatBudgetValue } from '../../../../../helper/common-function';
import Input from '../../../../ui/Input';
import SiteNavigateIcon from '../../../../menu/icons/siteNavigateIcon';
import PersonIcon from '../../../../menu/icons/personIcon';
import Button from '../../../../ui/Button';
import NewViewIcon from '../../../../menu/icons/newViewIcon';
import NewApproveIcon from '../../../../menu/icons/newApproveIcon';
import NewRejectIcon from '../../../../menu/icons/newRejectIcon';

const ExpenseDetailApprove: React.FC = (props: any) => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID = encryptedData.userId;
  const params = useParams();
  const navigate = useNavigate();
  const projectId = Number(params?.projectId);
  const expenseId = Number(params?.id);
  const expenseIdFromProps = props?.expenseID;

  const [tableData, setTableData] = useState<any>([]);
  const [value, setValue] = useState(0);
  const [openApprove, setOpenApprove] = useState(false);
  const [openReject, setOpenReject] = useState(false);
  const [openComplete, setOpenComplete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [reload, setReload] = useState(false);
  const nullLableNameFromEnv = `${environment.NULLVALUE}`;
  const { mutate: updateSiteExpenseData } = updatesiteExpenseStatus();
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
    if (expenseIdFromProps === undefined) {
      const fetchData = async () => {
        const datas = await siteExpenseService.getOnesiteExpenseByID(expenseId);
        setTableData(datas.data);
      };
      fetchData();
    } else {
      const fetchData = async () => {
        const datas = await siteExpenseService.getOnesiteExpenseByID(
          expenseIdFromProps
        );
        setTableData(datas.data);
      };
      fetchData();
    }
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
    if (value !== 0) fetchData();
  }, [reload, value]);

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const approveHandler = (id: any) => {
    setValue(id);
    setOpenApprove(true);
    setReload(false);
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

  const approveSite = async () => {
    const object: any = {
      updated_by: userID,
      expense_id: tableData.expense_id,
      status: 'Completed',
      progressed_by: userID,
    };
    updateSiteExpenseData(object, {
      onSuccess(data, variables, context) {
        if (data?.message === 'success') {
          setMessage('Process has been Completed');
          setOpenSnack(true);
          setReload(true);
          handleCloseConfirm();
        }
      },
    });
  };

  const hanldeOpen = () => {
    setOpenComplete(true);
  };
  const handleCloseConfirm = () => {
    setOpenComplete(false);
  };

  return (
    <div>
      <div>
        {!props.expenseID ? (
          <ProjectSubheader
            title="Claim Details"
            navigation={'/site-expense-approve'}
            description=""
          />
        ) : (
          ''
        )}
      </div>
      <div className={Styles.sub_header}>
        <div style={{ display: 'flex' }}>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              padding: '20px 10px 20px 30px',
            }}
          >
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Expense Code</span>
              <h3>{tableData?.expense_code}</h3>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: '10px',
              padding: '20px 10px 20px 10px',
            }}
          >
            <div>
              <SiteNavigateIcon width={30} height={30} />
            </div>
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Site</span>
              <span style={{ width: '160px' }}>
                {tableData?.site_data?.name}
              </span>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div style={{ display: 'flex', alignItems: 'center', gap: '20px' }}>
            <div>
              <PersonIcon width={30} height={30} />
            </div>
            <div className={Styles.textContent_1}>
              <span className={Styles.projectTitle}>Applied By</span>
              <span style={{ width: '160px' }}>{tableData?.employee_name}</span>
            </div>
          </div>
        </div>
        <div className={Styles.boqAmount}>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: '10px',
              padding: '20px 10px 20px 10px',
            }}
          >
            <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  tableData?.approved_total ? tableData?.approved_total : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Approved Amount</span>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: '10px',
              padding: '20px 10px 20px 10px',
            }}
          >
            <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  tableData?.rejected_total ? tableData?.rejected_total : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Rejected Amount</span>
            </div>
          </div>
          <div className={Styles.lineStyles}>
            <div className={Styles.vertical}>
              <div className={Styles.verticalLine}></div>
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: '10px',
              padding: '20px 10px 20px 10px',
            }}
          >
            <div className={Styles.countContent}>
              <h3>
                {formatBudgetValue(
                  tableData?.total_amount ? tableData?.total_amount : 0
                )}
              </h3>
              <span className={Styles.countContentTitle}>Total Amount</span>
            </div>
          </div>
        </div>
      </div>
      <div className={Styles.selected}></div>
      {props.expenseID ? (
        ''
      ) : tableData?.isEnableComplete !== false ? (
        <div className={Styles.completeButton}>
          <Button
            color="primary"
            shape="rectangle"
            justify="center"
            size="small"
            type="submit"
            onClick={hanldeOpen}
            disabled={tableData?.status === 'Completed'}
          >
            Review Complete
          </Button>
        </div>
      ) : (
        ''
      )}
      <div className={Styles.tableContainerBottom}>
        <table className={Styles.scrollable_table}>
          <thead className="globaltablehead">
            <tr>
              <th>#</th>
              <th>Description</th>
              <th>Expense Name</th>
              <th>Amount</th>
              <th>Documents</th>
              <th>Status</th>
              {props.expenseID ? (
                ''
              ) : tableData?.status === 'InProgress' ? (
                <th>Action</th>
              ) : tableData?.status === 'Pending' ? (
                <th>Action</th>
              ) : (
                ''
              )}
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
                            : data?.status === 'Pending'
                            ? Styles.pendingStatus
                            : ''
                        }`}
                      >
                        {data?.status || nullLableNameFromEnv}
                      </span>
                    </td>
                    {props.expenseID ? (
                      ''
                    ) : tableData?.status === 'InProgress' ? (
                      <td className={Styles.tableIcon}>
                        {/* <NewViewIcon /> */}
                        <NewApproveIcon
                          onClick={() =>
                            approveHandler(data.expense_details_id)
                          }
                        />
                        <NewRejectIcon
                          onClick={() => rejectHandler(data.expense_details_id)}
                        />
                      </td>
                    ) : tableData?.status === 'Pending' ? (
                      <td className={Styles.tableIcon}>
                        {/* <NewViewIcon /> */}
                        <NewApproveIcon
                          onClick={() =>
                            approveHandler(data.expense_details_id)
                          }
                        />
                        <NewRejectIcon
                          onClick={() => rejectHandler(data.expense_details_id)}
                        />
                      </td>
                    ) : (
                      ''
                    )}
                  </tr>
                );
              }
            })}
          </tbody>
        </table>
        {props.expenseID && (
          <div className={Styles.propCancel}>
            <Button
              type="button"
              color="secondary"
              shape="rectangle"
              size="small"
              justify="center"
              onClick={() => props.setOpen(false)}
            >
              Close
            </Button>
          </div>
        )}
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
      <CustomConfirm
        open={openComplete}
        title=""
        contentLine1="By clicking submit you cannot edit this expense!"
        handleClose={handleCloseConfirm}
        handleConfirm={() => approveSite(tableData?.expense_id)}
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
