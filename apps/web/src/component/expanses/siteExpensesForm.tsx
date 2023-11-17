import React, { useEffect, useState } from 'react';
import Styles from '../../styles/expanses.module.scss';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useFormik } from 'formik';
import {
  useGetBymasertDataType,
  useGetBymasertDataTypeDrop,
} from '../../hooks/masertData-hook';
import PopupExpense from './popupExpanse';
import CustomDelete from '../ui/customDeleteDialogBox';
import {
  useCreatesiteExpense,
  useUpdatesiteExpense,
} from '../../hooks/expense-hook';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import siteExpenseService from '../../service/expense-service';
import { format } from 'date-fns';
import * as Yup from 'yup';
import { getCreateValidateyup } from '../../helper/constants/siteExpanse-constants';
import CustomDialogBox from '../ui/CustomDialog';
import CustomSnackBar from '../ui/customSnackBar';
import { useParams, useNavigate } from 'react-router-dom';
import SiteExpensesDetails from './siteExpensesDetails';
import { useGetProjectSite } from '../../hooks/project-hooks';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import SiteExpenseBill from './SiteExpensBill';
import CustomConfirm from '../ui/CustomConfirmDialogBox';
import ProjectSubheader from '../project/projectSubheader';

const SiteExpensesForm = () => {
  const params = useParams();
  const navigate = useNavigate();
  const projectId = Number(params?.projectId);
  const siteId = Number(params?.siteId);
  const { data: getSiteList } = useGetProjectSite(Number(projectId));
  const validationSchema = getCreateValidateyup(Yup);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const tableInputwidth = '100px';
  const [expenseList, setExpenseList] = useState<any>([]);
  const [expense, setExpense] = useState();
  const [ExpenseValue, setExpenseValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
  const [reload, setReload] = useState(false);
  const [errors, setErrors] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [openConfirm, setOpenConfirm] = useState(false);
  const [expenseBill, setExpenseBill] = useState<any>([]);
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
    submitType: '',
  });
  const handleCloseConfirm = () => {
    setOpenConfirm(false);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const handleCloseDialog = () => {
    setOpenDialog(false);
  };
  /* Function for closing the snackbar */
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const drafthandler = () => {
    formik.setFieldValue('submitType', 'Draft');
    formik.submitForm();
  };
  const { data: getAllDiscription } = useGetBymasertDataType('SEDT');
  const { data: getAllSiteDepartment } = useGetBymasertDataType('SITD');
  const { data: getAllpurpose } = useGetBymasertDataType('SITP');
  const { data: getAlldesignation } = useGetBymasertDataType('SITDG');
  const { data: getSiteExpense } = useGetBymasertDataTypeDrop('SIEP');
  const { mutate: postSiteExpenseData, isLoading: postLoader } =
    useCreatesiteExpense();
  const { mutate: updateSiteExpenseData, isLoading: updateLoader } =
    useUpdatesiteExpense();

  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  useEffect(() => {
    const fetchData = async () => {
      const postIds = {
        projectId: projectId,
        siteId: siteId,
      };
      const datas = await siteExpenseService.getOnesiteExpenseByID(params?.id);
      console.log('datas', datas);
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
        submitType: datas?.data?.status,
      });
    };
    if (params?.id !== undefined) fetchData();
  }, [reload]);

  const submitHandler = () => {
    setOpenConfirm(true);
  };
  const handleConfirmForm = () => {
    formik.setFieldValue('submitType', 'Pending');
    formik.submitForm();
    setOpenConfirm(false);
  };
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      const statusData = values.submitType === 'Draft' ? 'Draft' : 'Pending';
      let count = 0;
      for (let i = 0; i < expenseList.length; i++) {
        if (expenseList[i].is_delete === false) {
          count++;
        }
      }
      if (count === 0) {
        setOpenDialog(true);
      } else {
        if (values.expense_id === '') {
          const object: any = {
            site_id: values.site_id,
            project_id: projectId,
            employee_name: values.employee_name,
            employee_id: values.employee_id,
            employee_phone: values.employee_phone,
            end_date: values.end_date,
            start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            expense_details: expenseList,
            created_by: encryptedData?.userId,
            bill_details: expenseBill,
            status: statusData,
          };
          console.log('objectpost', object);
          postSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              console.log('data', data);
              if (data?.status === true) {
                setMessage('Site Expense has been added successfully !');
                setOpenSnack(true);
                setTimeout(() => {
                  navigate(`/project-edit/${projectId}`);
                }, 3000);
              }
            },
          });
        } else {
          const object: any = {
            site_id: values.site_id,
            project_id: projectId,
            employee_name: values.employee_name,
            employee_id: values.employee_id,
            employee_phone: values.employee_phone,
            end_date: values.end_date,
            start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            expense_details: expenseList,
            created_by: encryptedData?.userId,
            updated_by: encryptedData?.userId,
            expense_id: values.expense_id,
            bill_details: expenseBill,
            status: statusData,
          };
          updateSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              if (data?.status === true) {
                setMessage('Site Expense has been updated successfully !');
                setOpenSnack(true);
                setTimeout(() => {
                  navigate(`/project-edit/${projectId}`);
                }, 3000);
              }
            },
          });
        }
      }
    },
  });
  return (
    <div>
      <div className={Styles.container}>
        <ProjectSubheader
          description={
            params?.id !== undefined
              ? ' Review, update, or modify your site-related expenses'
              : ' Provide all the necessary details for each expense'
          }
          navigation={`/project-edit/${projectId}`}
          title={
            params?.id !== undefined ? 'Edit Site Expense' : 'Add Site Expense'
          }
        />
      </div>
      <div className={Styles.dividerStyle}></div>
      <form>
        <div className={Styles.box}>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <AutoCompleteSelect
                  name="site_id"
                  label="Site"
                  mandatory={true}
                  optionList={getSiteList !== undefined ? getSiteList : []}
                  value={formik.values.site_id}
                  onChange={formik.handleChange}
                  onSelect={(value) => {
                    formik.setFieldValue('site_id', value);
                  }}
                  error={formik.touched.site_id && formik.errors.site_id}
                  disabled={params?.id ? true : false}
                />
              </div>
            </div>
          </div>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="EMP Name"
                  name="employee_name"
                  onChange={formik.handleChange}
                  value={formik.values.employee_name}
                  mandatory
                  error={
                    formik.touched.employee_name && formik.errors.employee_name
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Input
                  label="EMP ID"
                  name="employee_id"
                  onChange={formik.handleChange}
                  value={formik.values.employee_id}
                  mandatory
                  error={
                    formik.touched.employee_id && formik.errors.employee_id
                  }
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="EMP Phone"
                  name="employee_phone"
                  onChange={formik.handleChange}
                  value={formik.values.employee_phone}
                  error={
                    formik.touched.employee_phone &&
                    formik.errors.employee_phone
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  label="Purpose"
                  defaultLabel="Select a Purpose"
                  placeholder="Select a Purpose"
                  name="purpose"
                  onChange={formik.handleChange}
                  value={formik.values.purpose}
                  mandatory
                  error={formik.touched.purpose && formik.errors.purpose}
                >
                  {getAllpurpose?.map((option: any) => (
                    <option
                      key={option.master_data_name}
                      value={option.master_data_name}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Select
                  label="Department"
                  defaultLabel="Select a Department"
                  placeholder="Select a Department"
                  name="department"
                  onChange={formik.handleChange}
                  value={formik.values.department}
                  mandatory
                  error={formik.touched.department && formik.errors.department}
                >
                  {getAllSiteDepartment?.map((option: any) => (
                    <option
                      key={option.master_data_name}
                      value={option.master_data_name}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  label="Designation"
                  name="designation"
                  defaultLabel="Select a Designation"
                  placeholder="Select a Designation"
                  onChange={formik.handleChange}
                  value={formik.values.designation}
                  error={
                    formik.touched.designation && formik.errors.designation
                  }
                >
                  {getAlldesignation?.map((option: any) => (
                    <option
                      key={option.master_data_name}
                      value={option.master_data_name}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <DatePicker
                  label="Start Date"
                  name="start_date"
                  onChange={formik.handleChange}
                  value={formik.values.start_date}
                  mandatory
                  error={formik.touched.start_date && formik.errors.start_date}
                />
              </div>
              <div className={Styles.fieldStyle}>
                <DatePicker
                  label="End Date"
                  name="end_date"
                  onChange={formik.handleChange}
                  value={formik.values.end_date}
                  mandatory
                  error={formik.touched.end_date && formik.errors.end_date}
                />
              </div>
            </div>
          </div>
        </div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Expense</h3>
            <span className={Styles.content}>List your site expense.</span>
          </div>
          <div>
            <SiteExpensesDetails
              setExpenseList={setExpenseList}
              expenseList={expenseList}
              setMessage={setMessage}
              setOpenSnack={setOpenSnack}
            />
          </div>
        </div>
        <div>
          <div className={Styles.textContent}>
            <h3>Expense Bill</h3>
            <span className={Styles.content}>Add your expense bill.</span>
          </div>
          <div>
            <SiteExpenseBill
              projectId={projectId}
              setExpenseBill={setExpenseBill}
              expenseBill={expenseBill}
            />
          </div>
        </div>
        <div className={Styles.submitButton} style={{ gap: '50px' }}>
          <Button
            className={Styles.resetButton}
            type="button"
            shape="rectangle"
            size="small"
            justify="center"
            onClick={() => drafthandler()}
          >
            Draft
          </Button>
          <Button
            color="primary"
            shape="rectangle"
            justify="center"
            size="small"
            type="button"
            onClick={() => submitHandler()}
          >
            Submit
          </Button>
        </div>
      </form>
      {/* <CustomDelete
        open={openDelete}
        title="Delete Site Expense"
        contentLine1="Are you sure you want to delete this Expense ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteSiteExpense}
      /> */}
      <CustomConfirm
        open={openConfirm}
        title="Confirm Submit"
        contentLine1="If you confirmed this site expense  will move to the review process"
        handleClose={handleCloseConfirm}
        handleConfirm={handleConfirmForm}
      />
      <CustomDialogBox
        open={openDialog}
        title="Warning"
        contentLine1="Please add site expanse details"
        contentLine2=""
        handleClose={handleCloseDialog}
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

export default SiteExpensesForm;
