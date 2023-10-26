import React, { useState } from 'react';
import Input from '../ui/Input';
import Button from '../ui/Button';
import ProjectSubheader from '../project/projectSubheader';
import Styles from '../../styles/newStyles/expenseRecall.module.scss';
import siteExpenseService from '../../service/expense-service';
import { formatBudgetValue } from '../../helper/common-function';
import ApproveDialogBox from '../ui/CustomApprovePopup';
import ApproveCommentDialogBox from '../ui/ApproveCommentPopup';
import { createExpenseRecall } from '../../hooks/expense-recall-hooks';
import CustomSnackBar from '../ui/customSnackBar';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const ExpenseRecall = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID = encryptedData.userId;
  const { mutate: createExpenseRecallData } = createExpenseRecall();
  const [value, setValue] = useState('');
  const [expenseValue, setExpenseValue] = useState(0);
  const [searchData, setSearchData] = useState(false);
  const [tableData, setTableData] = useState(null);
  const [openApprove, setOpenApprove] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [reload, setReload] = useState(false);
  const [warning, setWarning] = useState(false);
  const currentDate = new Date();

  let rowindex = 0;
  const handleSearch = async () => {
    const data = await siteExpenseService.getOnesiteExpenseByCode(value);
    if (data.message === 'success') {
      setTableData(data.data);
      setSearchData(true);
    } else if (data.data === null) {
      setMessage('No relevant data found for the provided expense code');
      setOpenSnack(true);
      setWarning(true);
      setTableData(null);
      setSearchData(true);
    }
  };

  const handleClear = () => {
    setValue('');
    setTableData(null);
    setSearchData(false);
  };

  const handleChange = (event: any) => {
    setValue(event.target.value);
  };

  const approveHandler = (id: any) => {
    setExpenseValue(id);
    setOpenApprove(true);
    setReload(false);
  };
  const handleCloseApprove = () => {
    setOpenApprove(false);
  };
  const handleSnackBarClose = () => {
    setOpenSnack(false);
    setWarning(false);
  };
  const approveExpense = (comments: string) => {
    const object: any = {
      expense_details_id: expenseValue,
      site_id: tableData?.['site_id'],
      expense_id: tableData?.['expense_id'],
      reason: comments,
      is_recalled: true,
      recall_creator_id: userID,
      created_by: userID,
      recall_date: currentDate.toISOString().slice(0, 10),
    };
    console.log('object', object);
    createExpenseRecallData(object, {
      onSuccess(data, variables, context) {
        if (data?.message === 'success') {
          setMessage('Site Expense Recall created');
          setOpenSnack(true);
          setReload(true);
          setSearchData(true);
          handleCloseApprove();
          handleSearch();
        }
      },
    });
  };

  return (
    <div>
      <div>
        <ProjectSubheader
          title="Expense Reversal"
          navigation={'/home'}
          description=""
        />
      </div>
      <div className={Styles.mainDiv}>
        <Input
          name="expense_id"
          label="Expense Code"
          placeholder="Enter Expense Code"
          value={value}
          onChange={handleChange}
          width="300px"
        />
        <div className={Styles.submitButton}>
          <Button
            color="primary"
            shape="rectangle"
            justify="center"
            size="small"
            onClick={handleSearch}
            disabled={value === ''}
          >
            Submit
          </Button>
          <Button
            color="secondary"
            shape="rectangle"
            justify="center"
            size="small"
            onClick={handleClear}
            disabled={value === ''}
          >
            Clear
          </Button>
        </div>
      </div>
      {tableData !== null ? (
        <div className={Styles.tableContainerBottom}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>#</th>
                <th>Description</th>
                <th>Expense Name</th>
                <th>Amount</th>
                <th>Documents</th>
                <th>Status</th>
                <th>Action</th>
              </tr>
            </thead>
            <tbody>
              {tableData &&
                tableData?.['expense_details']?.map((data: any, index: any) => {
                  if (data?.is_delete === false) {
                    rowindex = rowindex + 1;
                    return (
                      <tr>
                        <td>{rowindex}</td>
                        <td>{data?.description}</td>
                        <td>{data?.expense_master_data?.master_data_name}</td>
                        <td>{formatBudgetValue(data?.total)}</td>
                        <td>
                          {data?.bill_details.length > 0
                            ? data?.bill_details?.map(
                                (files: any, index: any) => (
                                  <ol key={index}>
                                    <a href={files.path}>
                                      Document {index + 1}
                                    </a>
                                  </ol>
                                )
                              )
                            : ''}
                        </td>
                        <td>
                          <span>{data?.status}</span>
                        </td>
                        {data?.is_recalled === false ? (
                          data?.status === 'Rejected' ? (
                            <td>
                              <span className={Styles.rejectedStatus}>
                                Invalid
                              </span>
                            </td>
                          ) : (
                            <td>
                              <Button
                                color="secondary"
                                shape="rectangle"
                                justify="center"
                                size="small"
                                onClick={() =>
                                  approveHandler(data.expense_details_id)
                                }
                              >
                                Recall
                              </Button>
                            </td>
                          )
                        ) : (
                          <td>
                            <span className={Styles.pendingStatus}>
                              Recalled
                            </span>
                          </td>
                        )}
                      </tr>
                    );
                  }
                })}
            </tbody>
          </table>
        </div>
      ) : searchData === true && tableData === null ? (
        <div className={Styles.emptyDataHandling}>
          <div className={Styles.emptyDataHandling}>
            <img src="/nodata.jpg" alt="aa" width="40%" height="40%" />
          </div>
          <div>
            <h5>No relevant data found for the provided expense code</h5>
          </div>
        </div>
      ) : (
        <div className={Styles.emptyDataHandling}>
          <div className={Styles.emptyDataHandling}>
            <img src="/reverse.jpg" alt="aa" width="30%" height="30%" />
          </div>
          <div></div>
        </div>
      )}
      <ApproveCommentDialogBox
        open={openApprove}
        title="Recall Site Expense"
        contentLine1="Are you sure want to recall this site expense ?"
        contentLine2=""
        handleClose={handleCloseApprove}
        onReject={approveExpense}
      />
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type={warning ? 'error' : 'success'}
      />
    </div>
  );
};
export default ExpenseRecall;
