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
import AddIcon from '../../../menu/icons/addIcon';
import MoneyIcon from '../../../menu/icons/moneyIcon';

const ProjectSiteExpenseForm: React.FC = (props: any) => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const projectId = Number(props?.projectId);
  const siteId = Number(props?.siteId);
  const currentDate = new Date();
  const [expenseList, setExpenseList] = useState<any>([]);
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
    expenseList: [],
  });
  const [errors, setErrors] = useState<any>();
  const validationSchema = Yup.object().shape({});
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [openConfirm, setOpenConfirm] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
  const [loader, setLoader] = useState(false);
  const [expenseBill, setExpenseBill] = useState<any>([]);
  const [totalAmount, setTotalAmount] = useState<any>();
  // const [checked, setChecked] = useState(false);
  const [tableView, setTableView] = useState(false);
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
        purpose: datas?.data?.purpose,
        department: datas?.data?.department,
        designation: datas?.data?.designation,
        site_expense_id: datas?.data?.site_expense_id,
        expense_id: datas?.data?.expense_id,
        site_id: datas?.data?.site_id,
        submitType: datas?.data?.status,
        total_amount: datas?.data?.total_amount,
        expense_code: datas?.data?.expense_code,
        expenseList: datas?.data?.expense_details,
      });
    };

    if (props?.mode === 'Edit') fetchData();
  }, [props?.expenseID, props?.mode]);
  useEffect(() => {
    formik.setFieldValue('expenseList', [...expenseList]);
    const totalSelectedPrice = expenseList.reduce((total: any, item: any) => {
      if (item.is_delete === false) {
        return total + item.total;
      }
      return total;
    }, 0);
    setTotalAmount(Number(totalSelectedPrice));
  }, [expenseList]);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      const statusData = values.submitType === 'Draft' ? 'Draft' : 'Pending';
      let count = 0;
      const schema = Yup.array().of(
        Yup.object().shape({
          description: Yup.string().required('Description is required'),
          is_delete: Yup.string().required(),
          expense_data_id: Yup.string()
            .typeError('Site Expense is required')
            .required('Site Expense is required')
            .test(
              'description-availability',
              'Site Expense is already present',
              async function (value, { parent }: Yup.TestContext) {
                const isDelete = false;
                const { path, createError } = this;
                try {
                  let dummy: any = [];
                  const allIds = expenseList.map((item: any) => {
                    console.log('item', item);

                    if (item.is_delete === false)
                      dummy.push(item.expense_data_id);
                  });
                  console.log('allIds', dummy);
                  const checking = dummy.filter(
                    (id) => Number(id) === Number(value)
                  ).length;
                  if (checking <= 1) {
                    return true;
                  } else {
                    return false;
                  }
                } catch {
                  return true;
                }
              }
            ),
          total: Yup.number()
            .min(1, 'Amount must be more then 0')
            .max(100000, 'Amount must be less then 100000')
            .typeError('Only Numbers are allowed')
            .required('Amount is required'),
        })
      );
      await schema
        .validate(expenseList, { abortEarly: false })
        .then(async () => {
          console.log('IntialValuevalues', values);
          setErrors({});
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
                total_amount: Number(totalSelectedPrice),
              };
              postSiteExpenseData(object, {
                onSuccess(data, variables, context) {
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
                total_amount: Number(totalSelectedPrice),
              };
              updateSiteExpenseData(object, {
                onSuccess(data, variables, context) {
                  if (data?.status === true) {
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
        })
        .catch((e: any) => {
          let errorObj = {};
          e.inner?.map((error: any) => {
            console.log('error', e);
            return (errorObj[error.path] = error.message);
          });
          setErrors({
            ...errorObj,
          });
        });
    },
  });
  return (
    <div>
      <CustomLoader loading={loader} size={20}>
        <div className={Styles.container}>
          {props?.mode === 'Edit' || tableView ? (
            <div className={Styles.formContainer}>
              <div className={Styles.form_fields}>
                {props.expenseID && formik.values.expense_code ? (
                  <div className={Styles.expCode}>
                    <h4>Expense Code : </h4>
                    <p>{formik.values.expense_code || 'EXP-YYYY'}</p>
                  </div>
                ) : (
                  ''
                )}
                <div className={Styles.fields_container_1}>
                  <div className={Styles.fieldStyle}>
                    <DatePicker
                      label="Bill Date"
                      name="bill_date"
                      onChange={formik.handleChange}
                      value={formik.values.bill_date}
                      mandatory={true}
                      error={
                        formik.touched.bill_date && formik.errors.bill_date
                      }
                    />
                  </div>
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
          ) : (
            ''
          )}
          {props?.mode === 'Edit' || tableView ? (
            <div className={Styles.tableContainer}>
              <SiteExpensesDetails
                setExpenseList={setExpenseList}
                expenseList={expenseList}
                initialValues={initialValues}
                setInitialValues={setInitialValues}
                setMessage={setMessage}
                setOpenSnack={setOpenSnack}
                siteId={Number(props?.siteId)}
                loader={loader}
                setLoader={setLoader}
                setTotalAmount={setTotalAmount}
                totalAmount={totalAmount}
                mode={props?.mode}
                errors={errors}
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
          ) : (
            <div>
              <div className={Styles.addNewRowView}>
                <MoneyIcon height={50} width={50} color="#475467" />
                <h5>No Site Claims added for this site </h5>
                <span className={Styles.spanContent}>
                  Let's add an claim now
                </span>
                <Button
                  type="button"
                  color="primary"
                  shape="rectangle"
                  size="small"
                  justify="center"
                  icon={<AddIcon color="white" />}
                  onClick={() => setTableView(true)}
                >
                  Add Claim
                </Button>
              </div>
            </div>
          )}
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
          contentLine1="Please add site claim details"
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
