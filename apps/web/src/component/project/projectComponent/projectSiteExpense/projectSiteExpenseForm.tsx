import React, { useEffect, useState } from 'react';
import SiteExpensesForm from '../../../expanses/siteExpensesForm';
import Styles from '../../../../styles/newStyles/projectSiteExpense.module.scss';
import AutoCompleteSelect from '../../../ui/AutoCompleteSelect';
import { useFormik } from 'formik';
import {
  createsiteExpense,
  updatesiteExpense,
} from '../../../../hooks/expense-hook';
import { format } from 'date-fns';
import * as Yup from 'yup';
import { getCreateValidateyup } from '../../../../helper/constants/siteExpanse-constants';
import { store, RootState } from '../../../../redux/store';
import { getToken } from '../../../../redux/reducer';
import { getProjectSite } from '../../../../hooks/project-hooks';
import DatePicker from '../../../ui/CustomDatePicker';
import SiteExpensesDetails from './siteExpensesDetails';
import Input from '../../../ui/Input';
import TickIcon from '../../../menu/icons/tickIcon';
import EnrichIcon from '../../../menu/icons/enrichIcon';
import FlagIcon from '../../../menu/icons/flagIcon';
import Button from '../../../ui/Button';
import Checkbox from '../../../ui/Checkbox';
import CustomConfirmDialogBox from '../../../ui/CustomConfirmDialogBox';
import CustomDialogBox from '../../../ui/CustomDialog';
import siteExpenseService from '../../../../service/expense-service';
import CustomSnackbar from '../../../ui/customSnackBar';
import { formatBudgetValue } from '../../../../helper/common-function';
import SiteExpenseBill from './SiteExpensBill';
import CustomLoader from '../../../ui/customLoader';
import CurrencyIcon from '../../../menu/icons/CurrencyIcon';

const ProjectSiteExpenseForm: React.FC = (props: any) => {
  // console.log('filterValue', props);
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  // console.log('encryptedData', encryptedData?.userData?.contact_no);
  const projectId = Number(props?.projectId);
  const siteId = Number(props?.siteId);
  // console.log('dddddd', siteId);
  const currentDate = new Date();
  const [initialValues, setInitialValues] = useState({
    employee_name:
      encryptedData?.userData?.first_name +
      ' ' +
      encryptedData?.userData?.last_name,
    employee_id: '',
    employee_phone:
      encryptedData?.userData?.contact_no != null
        ? encryptedData?.userData?.contact_no
        : '',
    // end_date: '',
    // start_date: '',
    purpose: '',
    department: '',
    designation: '',
    site_expense_id: '',
    site_id: siteId,
    expense_id: '',
    submitType: '',
    total_amount: '',
    expense_code: '',
    bill_date: currentDate.toISOString().slice(0, 10),
  });
  const [expenseList, setExpenseList] = useState<any>([]);
  const validationSchema = getCreateValidateyup(Yup);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [openConfirm, setOpenConfirm] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
  const [loader, setLoader] = useState(false);
  const [expenseBill, setExpenseBill] = useState<any>([]);
  const [totalAmount, setTotalAmount] = useState<any>();
  const [checked, setChecked] = useState(false);
  const { data: getSiteList } = getProjectSite(Number(projectId));
  const { mutate: postSiteExpenseData, isLoading: postLoader } =
    createsiteExpense();
  const { mutate: updateSiteExpenseData, isLoading: updateLoader } =
    updatesiteExpense();
  const drafthandler = () => {
    formik.setFieldValue('submitType', 'Draft');
    formik.submitForm();
  };
  const submitHandler = () => {
    setOpenConfirm(true);
  };
  const handleCloseDialog = () => {
    setOpenDialog(false);
  };
  const handleCloseConfirm = () => {
    setOpenConfirm(false);
  };
  const handleConfirmForm = () => {
    formik.setFieldValue('submitType', 'Inprogress');
    formik.submitForm();
    setOpenConfirm(false);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

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
      const datas = await siteExpenseService.getOnesiteExpenseByID(
        props?.expenseID
      );
      // console.log('datas', datas);
      const arry: any = [];
      setExpenseBill(
        datas?.data?.bill_details === null ? [] : datas?.data?.bill_details
      );
      setExpenseList(datas?.data?.expense_details);
      setInitialValues({
        employee_name: datas?.data?.employee_name,
        employee_id: datas?.data?.employee_id,
        employee_phone: datas?.data?.employee_phone,
        bill_date: dateFormat(datas?.data?.bill_date),
        // end_date: dateFormat(datas?.data?.end_date),
        // start_date: dateFormat(datas?.data?.start_date),
        purpose: datas?.data?.purpose,
        department: datas?.data?.department,
        designation: datas?.data?.designation,
        site_expense_id: datas?.data?.site_expense_id,
        expense_id: datas?.data?.expense_id,
        site_id: datas?.data?.site_id,
        submitType: datas?.data?.status,
        total_amount: datas?.data?.total_amount,
        expense_code: datas?.data?.expense_code,
      });
    };

    if (props?.mode === 'Edit') fetchData();
  }, [props?.expenseID, props?.mode]);
  useEffect(() => {
    const totalSelectedPrice = expenseList.reduce((total: any, item: any) => {
      if (item.is_delete === false) {
        return total + item.total;
      }
      return total;
    }, 0);
    // console.log('totalSelectedPrice', totalSelectedPrice);
    setTotalAmount(Number(totalSelectedPrice));
  }, [expenseList]);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      // console.log('values', values);

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
        const totalSelectedPrice = expenseList.reduce(
          (total: any, item: any) => {
            if (item.is_delete === false) {
              return total + item.total;
            }
            return total;
          },
          0
        );
        if (values.expense_id === '') {
          const object: any = {
            site_id: values.site_id,
            project_id: projectId,
            employee_name:
              encryptedData?.userData?.first_name +
              ' ' +
              encryptedData?.userData?.last_name,
            employee_id: '',
            employee_phone:
              encryptedData?.userData?.contact_no != null
                ? encryptedData?.userData?.contact_no
                : '',
            bill_date: values.bill_date,
            // end_date: values.end_date,
            // start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            expense_details: expenseList,
            created_by: encryptedData?.userId,
            bill_details: expenseBill,
            status: statusData,
            total_amount: totalSelectedPrice,
          };
          // console.log('objectpost', object);
          postSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              // console.log('data', data);
              if (data?.status === true) {
                setMessage('Site Expense has been added successfully !');
                setOpenSnack(true);
                setTimeout(() => {
                  props.setOpen(!props.open);
                  props.setReload(!props.reload);
                }, 1000);
              }
            },
          });
        } else {
          // console.log('sumOfRates', totalSelectedPrice);
          const object: any = {
            site_id: values.site_id,
            project_id: projectId,
            employee_name:
              encryptedData?.userData?.first_name +
              ' ' +
              encryptedData?.userData?.last_name,
            employee_id: '',
            employee_phone:
              encryptedData?.userData?.contact_no != null
                ? encryptedData?.userData?.contact_no
                : '',
            bill_date: values.bill_date,
            // end_date: values.end_date,
            // start_date: values.start_date,
            purpose: values.purpose,
            department: values.department,
            designation: values.designation,
            expense_details: expenseList,
            created_by: encryptedData?.userId,
            updated_by: encryptedData?.userId,
            expense_id: values.expense_id,
            bill_details: expenseBill,
            status: statusData,
            total_amount: totalSelectedPrice,
          };
          // console.log('Editobject', object);
          updateSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              if (data?.status === true) {
                // console.log('editData', data);
                setMessage('Site Expense has been updated successfully !');
                setOpenSnack(true);
                setTimeout(() => {
                  props.setOpen(!props.open);
                  props.setReload(!props.reload);
                }, 1000);
              }
            },
          });
        }
      }
    },
  });
  return (
    <div>
      <CustomLoader loading={loader} size={20}>
        <div className={Styles.container}>
          <div className={Styles.formContainer}>
            <div className={Styles.form_fields}>
              {/* <div className={Styles.fields_container_1}> */}
              {/* <div className={Styles.fieldStyle}> */}
              {/* <Input
                    label="Expense Code"
                    placeholder="EXP-YYYY-"
                    name="quantity"
                    // mandatory={true}
                    disabled={true}
                    // width="350px"
                    value={formik.values.expense_code}
                    onChange={formik.handleChange}
                 
                  /> */}
              {/* </div> */}
              {/* <div className={Styles.fieldStyle}>
                <AutoCompleteSelect
                  name="site_id"
                  label="Site"
                  mandatory={true}
                  optionList={getSiteList != undefined ? getSiteList : []}
                  value={formik.values.site_id}
                  onChange={formik.handleChange}
                  onSelect={(value) => {
                    formik.setFieldValue('site_id', value);
                  }}
                  error={formik.touched.site_id && formik.errors.site_id}
                  disabled={siteId ? true : false}
                />
              </div> */}
              {/* <div className={Styles.fieldStyle}>
                  <Input
                    name="total_amount"
                    label="Total"
                    value={formatBudgetValue(
                      formik.values.total_amount
                        ? formik.values.total_amount
                        : 0
                    )}
                    onChange={formik.handleChange}
                    disabled
                    mandatory
                  />
                </div> */}
              {/* </div> */}
              {props.expenseID && formik.values.expense_code ? 
              <div className={Styles.expCode}>
                <h4>Expense Code : </h4>
                <p>{formik.values.expense_code || 'EXP-YYYY'}</p>
              </div>
               : ''}
              <div className={Styles.fields_container_1}>
                <div className={Styles.fieldStyle}>
                  <DatePicker
                    label="Bill Date"
                    name="bill_date"
                    onChange={formik.handleChange}
                    value={formik.values.bill_date}
                    mandatory={true}
                    error={formik.touched.bill_date && formik.errors.bill_date}
                    // disabled={checked === true ? false : true} 
                  />
                </div>
                {/* <div>
                  <Checkbox
                    checked={checked}
                    onChange={() => {
                      setChecked(!checked);
                    }}
                  />
                  <span style={{ fontSize: '70%', paddingLeft: '5px' }}>
                    Edit date?
                  </span>
                </div> */}
                {/* <div className={Styles.fieldStyle}>
                  <DatePicker
                    label="End Date"
                    name="end_date"
                    onChange={formik.handleChange}
                    value={formik.values.end_date}
                    mandatory
                    error={formik.touched.end_date && formik.errors.end_date}
                  />
                </div> */}
              </div>
              <div style={{ display: 'none' }}>
                <SiteExpenseBill
                  projectId={projectId}
                  setExpenseBill={setExpenseBill}
                  expenseBill={expenseBill}
                  loader={loader}
                  setLoader={setLoader}
                />
              </div>
            </div>
            <div>
              <CurrencyIcon width={90} height={90} color="#7f56d9" />
            </div>
          </div>
          <div className={Styles.tableContainer}>
            <SiteExpensesDetails
              setExpenseList={setExpenseList}
              expenseList={expenseList}
              setMessage={setMessage}
              setOpenSnack={setOpenSnack}
              siteId={Number(props?.siteId)}
              loader={loader}
              setLoader={setLoader}
              setTotalAmount={setTotalAmount}
              totalAmount={totalAmount}
              mode={props?.mode}
            />
            <div className={Styles.totalBudget}>
              <div>
                <span>Total : </span>
                {formatBudgetValue(Number(totalAmount))}
              </div>
            </div>
            <div className={Styles.buttonComponent}>
              <div className={Styles.dividerStyleOne}></div>
              <div className={Styles.bottomButton}>
                <Button
                  type="button"
                  color="primary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  onClick={() => submitHandler()}
                >
                  Save
                </Button>
                <Button
                  type="button"
                  color="secondary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  className={Styles.draftButton}
                  onClick={() => drafthandler()}
                >
                  Save Draft
                </Button>
                <Button
                  type="button"
                  color="secondary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  className={Styles.cancelButton}
                  onClick={() => props.setOpen(false)}
                >
                  Cancel
                </Button>
              </div>
            </div>
          </div>
        </div>
        <CustomConfirmDialogBox
          open={openConfirm}
          handleClose={handleCloseConfirm}
          handleConfirm={handleConfirmForm}
          title="Are you sure you want to save?"
          contentLine1="After saving the records you can't edit  "
        />
        <CustomDialogBox
          open={openDialog}
          title="Warning"
          contentLine1="Please add site expanse details"
          contentLine2=""
          handleClose={handleCloseDialog}
        />
        <CustomSnackbar
          open={openSnack}
          message={message}
          onClose={handleSnackBarClose}
          autoHideDuration={1000}
          type="success"
        />
      </CustomLoader>
    </div>
  );
};

export default ProjectSiteExpenseForm;
