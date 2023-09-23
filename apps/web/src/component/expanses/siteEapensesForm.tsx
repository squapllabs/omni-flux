import React, { useEffect, useState } from 'react';
import Styles from '../../styles/expanses.module.scss';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import DeleteIcon from '../menu/icons/deleteIcon';
import { useFormik } from 'formik';
import { getBymasertDataType } from '../../hooks/masertData-hook';
import PopupExpense from './popupExpanse';
import CustomDelete from '../ui/customDeleteDialogBox';
import {
  createsiteExpense,
  updatesiteExpense,
} from '../../hooks/siteExpanse-hooks';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import siteExpenseService from '../../service/siteExpanse-service';
import { format } from 'date-fns';
import * as Yup from 'yup';
import { getCreateValidateyup } from '../../helper/constants/siteExpanse-constants';
import CustomDialogBox from '../ui/CustomDialog';
import CustomSnackBar from '../ui/customSnackBar';
import { useParams, useNavigate } from 'react-router-dom';
import KeyboardBackspaceIcon from '../menu/icons/backArrow';
import SiteExpensesDetails from './siteExpensesDetails';

const SiteExpensesForm = () => {
  const params = useParams();
  const navigate = useNavigate();
  const projectId = Number(params?.projectId);
  const siteId = Number(params?.siteId);
  const validationSchema = getCreateValidateyup(Yup);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const tableInputwidth = '100px';
  let rowIndex = 0;
  const [expenseList, setExpenseList] = useState<any>([]);
  const [expense, setExpense] = useState();
  const [ExpenseValue, setExpenseValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
  const [reload, setReload] = useState(false);
  const [errors, setErrors] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
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
  });
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
  const { data: getAllDiscription } = getBymasertDataType('SEDT');
  const { data: getAllSiteDepartment } = getBymasertDataType('SITD');
  const { data: getAllpurpose } = getBymasertDataType('SITP');
  const { data: getAlldesignation } = getBymasertDataType('SITDG');
  const { mutate: postSiteExpenseData, isLoading: postLoader } =
    createsiteExpense();
  const { mutate: updateSiteExpenseData, isLoading: updateLoader } =
    updatesiteExpense();

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
      const datas = await siteExpenseService.getSiteExpenseByProjectandSiteID(
        postIds
      );

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
      });
      const arry: any = [];
      datas?.data?.site_expense_details.map((items: any) => {
        items.is_delete = 'N';
        const demo = {
          site_expense_details_id: items.site_expense_details_id,
          description: items.description,
          is_delete: 'N',
          air_transport: items.air_transport,
          fuel: items.fuel,
          labour_advance: items.labour_advance,
          phone_stationary: items.phone_stationary,
          food_snacks: items.food_snacks,
          purchase_service: items.purchase_service,
          others: items.others,
          total: items.total,
        };

        arry.push(demo);
      });
      setExpenseList(arry);
    };
    fetchData();
  }, [reload]);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      let count = 0;
      for (let i = 0; i < expenseList.length; i++) {
        if (expenseList[i].is_delete === 'N') {
          count++;
        }
      }
      if (count === 0) {
        setOpenDialog(true);
      } else {
        if (values.site_expense_id === '') {
          const object: any = {
            site_id: siteId,
            project_id: projectId,
            employee_name: values.employee_name,
            employee_id: values.employee_id,
            employee_phone: values.employee_phone,
            end_date: values.end_date,
            start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            site_expense_details: expenseList,
            created_by: encryptedData?.userId,
          };
          postSiteExpenseData(object, {
            onSuccess(data, variables, context) {
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
            site_id: siteId,
            project_id: projectId,
            employee_name: values.employee_name,
            employee_id: values.employee_id,
            employee_phone: values.employee_phone,
            end_date: values.end_date,
            start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            site_expense_details: expenseList,
            created_by: encryptedData?.userId,
            updated_by: encryptedData?.userId,
            site_expense_id: values.site_expense_id,
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
        <div className={Styles.top}>
          <div className={Styles.textContent}>
            <h3>Add Site Expense</h3>
            <span className={Styles.content}>Add your site expense.</span>
          </div>
          <div>
            <Button
              shape="rectangle"
              size="small"
              justify="center"
              color="primary"
              icon={<KeyboardBackspaceIcon />}
              onClick={() => {
                navigate('/settings');
              }}
            >
              Back
            </Button>
          </div>
        </div>
      </div>
      <form>
        <div className={Styles.box}>
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
            />
          </div>
          <div className={Styles.submitButton}>
            <Button
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
              type="button"
              onClick={formik.handleSubmit}
            >
              Submit
            </Button>
          </div>
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
