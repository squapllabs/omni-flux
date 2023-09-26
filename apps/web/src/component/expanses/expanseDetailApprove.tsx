import React, { useEffect, useState } from 'react';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import siteExpenseService from '../../service/expense-service';
import { format } from 'date-fns';
import CustomSnackBar from '../ui/customSnackBar';
import { useParams, useNavigate } from 'react-router-dom';
import SiteExpensesDetails from './siteExpensesDetails';
import CustomCard from '../ui/CustomCard';
import Styles from '../../styles/expenseApprove.module.scss';
import BackArrow from '../menu/icons/backArrow';
import Button from '../ui/Button';
import TickIcon from '../menu/icons/tickIcon';
import RejectIcon from '../menu/icons/cancelIcon';

const ExpenseDetailApprove = () => {
  const params = useParams();
  const navigate = useNavigate();
  const projectId = Number(params?.projectId);
  const expenseId = Number(params?.id);
  const [tableData, setTableData] = useState<any>([]);
  const [value,setValue] = useState(0);
  const [openApprove, setOpenApprove] = useState(false);
  const [openReject, setOpenReject] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [reload, setReload] = useState(false);
  const [expenseBill, setExpenseBill] = useState<any>([]);
  const [expenseList, setExpenseList] = useState<any>([]);
  const [initialValues, setInitialValues] = useState({
    expense_details_id: '',
    status: '',
    created_by:'',
    updated_by: ''
  });

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  let rowindex = 0;

  console.log('expenseId,projectId', expenseId, projectId);
  
  useEffect(() => {
    const fetchData = async () => {
      const datas = await siteExpenseService.getOnesiteExpenseByID(params?.id);
      setTableData(datas.data);
    };
    console.log('tableData', tableData);
    fetchData();
    if (expenseId !== undefined) fetchData();
  },[]);

  useEffect(() => {
    const fetchData = async () => {
      const data = await siteExpenseService.getOnesiteExpenseDetailByID(value);

    }
  })

  const approveHandler = (id: any) => {
    setValue(id);
    setOpenApprove(true);
  };


  return (
    <div>
      <div className={Styles.title}>
        <h2>Expense Detaills</h2>
        <Button
          type="button"
          color="primary"
          shape="rectangle"
          size="small"
          justify="center"
          icon={<BackArrow />}
          onClick={() => navigate('/settings')}
        >
          Back
        </Button>
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
      <div className={Styles.tableContainer}>
        <table>
          <thead>
            <tr>
              <th>SI No</th>
              <th>Expense Name</th>
              <th>Documents</th>
              <th>Action</th>
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
                    <td>{data?.expense_master_data?.master_data_name}</td>
                    <td>
                      {data?.bill_details?.map((files: any, index: any) => (
                        <ol key={index}>
                          <a href={files.path}>Document {index + 1}</a>
                        </ol>
                      ))}
                    </td>
                    <td>
                      <div className={Styles.tableIcon}>
                        <TickIcon onClick={() => approveHandler(data.expense_details_id)}/>
                        <RejectIcon />
                      </div>
                    </td>
                  </tr>
                );
              }
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
};
export default ExpenseDetailApprove;
