import React, { useEffect, useState } from 'react';
import Input from '../ui/Input';
import Button from '../ui/Button';
import ProjectSubheader from '../project/projectSubheader';
import Styles from '../../styles/newStyles/expenseRecall.module.scss';
import siteExpenseService from '../../service/expense-service';
import { formatBudgetValue } from '../../helper/common-function';
import ApproveDialogBox from '../ui/CustomApprovePopup';
import { updatesiteExpenseDetail } from '../../hooks/expense-hook';
import CustomSnackBar from '../ui/customSnackBar';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';

const ExpenseRecall = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID = encryptedData.userId;
  const { mutate: updateSiteExpenseDetailData } = updatesiteExpenseDetail();
  const [value, setValue] = useState('');
  const [expenseValue, setExpenseValue] = useState(0);
  const [searchData, setSearchData] = useState(false);
  const [tableData, setTableData] = useState(null);
  const [openApprove, setOpenApprove] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [reload, setReload] = useState(false);
  const [warning, setWarning] = useState(false);

  let rowindex = 0;
  const handleSearch = () => {
    setSearchData(true);
  };

  const handleClear = () => {
    setValue('');
    setTableData(null);
  };

  const handleChange = (event: any) => {
    setValue(event.target.value);
  };

  useEffect(() => {
    const fetchData = async () => {
      const data = await siteExpenseService.getOnesiteExpenseByCode(value);
      if (data.message === 'success') {
        setTableData(data.data);
        setSearchData(false);
      } else if (data.data === null) {
        setMessage('No relevant data found for the provided expense code');
        setOpenSnack(true);
        setWarning(true);
        setTableData(null);
        setSearchData(false);
      }
    };
    if (value !== '' && searchData === true) {
      fetchData();
    }
  }, [value, searchData, reload]);

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
  const approveExpense = () => {
    const object: any = {
      expense_details_id: expenseValue,
      is_recalled: true,
      updated_by: userID,
      progressed_by: userID,
    };
    updateSiteExpenseDetailData(object, {
      onSuccess(data, variables, context) {
        if (data?.status === true) {
          setMessage('Site Expense Detail has been Recalled');
          setOpenSnack(true);
          setReload(true);
          setSearchData(true);
          handleCloseApprove();
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
      {value !== '' ? (
        tableData?.status === 'Completed' ? (
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
                  <th>Action</th>
                </tr>
              </thead>
              <tbody>
                {tableData?.expense_details?.map((data: any, index: any) => {
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
                            <td></td>
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
        ) : (
          ''
        )
      ) : (
        <div className={Styles.emptyDataHandling}>
          <div>
            <img src="/imagereverse.jpg" alt="aa" />
          </div>
        </div>
      )}
      <ApproveDialogBox
        open={openApprove}
        title="Are you sure want to recall this site expense"
        contentLine1=""
        contentLine2=""
        handleClose={handleCloseApprove}
        handleConfirm={approveExpense}
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
