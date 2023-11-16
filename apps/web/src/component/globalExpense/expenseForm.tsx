import React, { useEffect, useState } from 'react';
import Styles from '../../styles/newStyles/globalExpense.module.scss';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useFormik } from 'formik';
import {
  useCreateGlobalExpense,
  useUpdateGlobalExpense,
} from '../../hooks/expense-hook';
import { format } from 'date-fns';
import * as Yup from 'yup';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import { useGetAllProjectDrop } from '../../hooks/project-hooks';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import CustomConfirmDialogBox from '../ui/CustomConfirmDialogBox';
import CustomDialogBox from '../ui/CustomDialog';
import siteExpenseService from '../../service/expense-service';
import CustomSnackbar from '../ui/customSnackBar';
import { formatBudgetValue } from '../../helper/common-function';
import SiteExpenseBill from '../project/projectComponent/projectSiteExpense/SiteExpensBill';
import CustomLoader from '../ui/customLoader';
import CurrencyIcon from '../menu/icons/CurrencyIcon';
import AddIcon from '../menu/icons/addIcon';
import MoneyIcon from '../menu/icons/moneyIcon';
import projectService from '../../service/project-service';
import Select from '../ui/selectNew';
import ExpensesDetailsForm from './expenseDetailForm';

const GlobalExpenseForm: React.FC = (props: any) => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userID = encryptedData.userId;
  const roleName =
    encryptedData?.userData?.user_roles[0]?.role_data?.role_name.toUpperCase();
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
    purpose: '',
    department: '',
    designation: '',
    site_expense_id: '',
    site_id: '',
    project_id: '',
    expense_id: '',
    submitType: '',
    total_amount: '',
    expense_code: '',
    bill_date: currentDate.toISOString().slice(0, 10),
    expenseList: [],
    type: '',
  });
  const [errors, setErrors] = useState<any>();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [openConfirm, setOpenConfirm] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
  const [loader, setLoader] = useState(false);
  const [expenseBill, setExpenseBill] = useState<any>([]);
  const [totalAmount, setTotalAmount] = useState<any>();
  const [projectValue, setProjectValue] = useState<any>();
  const [siteData, setSiteData] = useState<any>();
  const [tableView, setTableView] = useState(false);

  const { data: getProjectList = [], isLoading: dropLoading} = useGetAllProjectDrop();

  const { mutate: postSiteExpenseData, isLoading: postLoader } =
  useCreateGlobalExpense();

  const { mutate: updateSiteExpenseData, isLoading: updateLoader } =
  useUpdateGlobalExpense();
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
      const getData = await projectService.getOneProjectSite(projectValue);
      const arr: any = [];
      const siteList = getData?.data.map((site: any, index: any) => {
        const obj: any = {
          value: site?.site_id,
          label: site?.site_details?.name,
        };
        arr.push(obj);
      });
      setSiteData(arr);
    };
    if (projectValue !== undefined) fetchData();
  }, [projectValue]);

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
        project_id: datas?.data?.project_id,
        submitType: datas?.data?.status,
        total_amount: datas?.data?.total_amount,
        expense_code: datas?.data?.expense_code,
        expenseList: datas?.data?.expense_details,
        type: '',
      });
      setProjectValue(datas?.data?.project_id);
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

  const validationSchema =
    props?.mode !== 'Edit'
      ? Yup.object().shape({
          type: Yup.string().trim().required(' '),
          project_id: Yup.string().test(
            'Project is required',
            async function (value, { parent }: Yup.TestContext) {
              const modetype = parent.type;
              if (modetype === 'Project Based' && value === undefined) {
                return false;
              }
              return true;
            }
          ),
          site_id: Yup.string().test(
            'Site is required',
            async function (value, { parent }: Yup.TestContext) {
              const modeProject = parent.project_id;
              if (modeProject > 0 && value === undefined) {
                return false;
              }
              return true;
            }
          ),
        })
      : '';

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
            .required('Site Expense is required'),
          total: Yup.number()
            .min(1, 'Amount must be more then 0')
            .typeError('Only Numbers are allowed')
            .required('Amount is required')
            .test(
              'description-availability',
              '',
              async function (value, { parent }: Yup.TestContext) {
                let bill_type = parent.bill_type;
                if (bill_type === 'VOUCHER' && value > 5000) {
                  setMessage(
                    'In bill type voucher amount should not be more then 5000'
                  );
                  setOpenSnack(true);
                  return false;
                } else {
                  return true;
                }
              }
            ),
          bill_number: Yup.string(),
          bill_type: Yup.string().required('Bill type is required'),
          bill_details: Yup.array()
            .required()
            .test(
              'description-availability',
              'Site Expense is already present',
              async function (value, { parent }: Yup.TestContext) {
                let bill_details = parent.bill_details;
                if (
                  bill_details?.length < 0 &&
                  bill_details[0]?.is_delete === 'Y'
                ) {
                  return true;
                } else if (
                  bill_details?.length > 0 &&
                  bill_details[0]?.is_delete === 'N'
                ) {
                  return true;
                } else {
                  setMessage('Bill is Missing');
                  setOpenSnack(true);
                  return false;
                }
              }
            ),
        })
      );
      await schema
        .validate(expenseList, { abortEarly: false })
        .then(async () => {
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
            if (values.expense_id === '' && values.type === 'Project Based') {
              const object: any = {
                site_id: values?.site_id,
                project_id: values?.project_id,
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
                purpose: values.purpose,
                department: values.department,
                designation: values.designation,
                expense_details: expenseList,
                created_by: encryptedData?.userId,
                user_id: userID,
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
            } else if (values.expense_id === '' && values.type === 'General') {
              const object: any = {
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
                purpose: values.purpose,
                department: values.department,
                designation: values.designation,
                expense_details: expenseList,
                created_by: encryptedData?.userId,
                user_id: userID,
                bill_details: expenseBill,
                status: statusData,
                total_amount: Number(totalSelectedPrice),
              };
              postSiteExpenseData(object, {
                onSuccess(data, variables, context) {
                  if (data?.status === true) {
                    setMessage('Expense has been added successfully !');
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
                project_id: values.project_id,
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
                purpose: values.purpose,
                department: values.department,
                designation: values.designation,
                expense_details: expenseList,
                created_by: encryptedData?.userId,
                updated_by: encryptedData?.userId,
                expense_id: values.expense_id,
                bill_details: expenseBill,
                status: statusData,
                user_id: userID,
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

  const options: any = [
    { value: 'General', label: 'General' },
    { value: 'Project Based', label: 'Project Based' },
  ];

  return (
    <div>
      <CustomLoader
        loading={props.mode === 'Edit' ? updateLoader : postLoader}
        size={48}
        color="black"
      >
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
                  {props.mode === 'Edit' ? (
                    ''
                  ) : (
                    <div className={Styles.fieldStyle}>
                      <Select
                        width="180px"
                        name="type"
                        label="Type"
                        defaultLabel="Select from options"
                        placeholder="Select from options"
                        mandatory={true}
                        onChange={formik.handleChange}
                        value={formik.values.type}
                        error={formik.touched.type && formik.errors.type}
                      >
                        {options?.map((item: any, index: any) => {
                          return (
                            <option value={item.value}>{item.label}</option>
                          );
                        })}
                      </Select>
                    </div>
                  )}
                  <div className={Styles.fields_container_1}>
                    <div>
                      <AutoCompleteSelect
                        name="project_id"
                        label="Project"
                        placeholder="Select Project"
                        optionList={dropLoading === true ? [] : getProjectList}
                        value={formik?.values?.project_id}
                        error={
                          formik.values.type === 'Project Based'
                            ? formik?.touched?.project_id &&
                              formik?.errors?.project_id
                            : ''
                        }
                        onSelect={(value) => {
                          formik.setFieldValue('project_id', value);
                          // fetchProjectSiteData(value);
                          setProjectValue(value);
                        }}
                        mandatory={
                          formik?.values?.type === 'Project Based'
                            ? true
                            : false
                        }
                        width="200px"
                      />
                    </div>
                    <div>
                      <AutoCompleteSelect
                        name="site_id"
                        label="Site"
                        placeholder="Select Site"
                        value={formik?.values?.site_id}
                        optionList={siteData === undefined ? [] : siteData}
                        onSelect={(value) => {
                          formik.setFieldValue('site_id', value);
                        }}
                        error={
                          formik.values.type === 'Project Based'
                            ? formik.touched.site_id && formik.errors.site_id
                            : ''
                        }
                        mandatory={
                          formik?.values?.type === 'Project Based'
                            ? true
                            : props.mode === 'Edit'
                            ? false
                            : true
                        }
                        width="200px"
                      />
                    </div>
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
              <div style={{ paddingRight: '20px' }}>
                <CurrencyIcon width={90} height={90} color="#7f56d9" />
              </div>
            </div>
          ) : (
            ''
          )}
          {props?.mode === 'Edit' || tableView ? (
            <div className={Styles.tableContainer}>
              <ExpensesDetailsForm
                setExpenseList={setExpenseList}
                expenseList={expenseList}
                initialValues={initialValues}
                setInitialValues={setInitialValues}
                setMessage={setMessage}
                setOpenSnack={setOpenSnack}
                siteId={Number(initialValues?.site_id)}
                loader={loader}
                setLoader={setLoader}
                setTotalAmount={setTotalAmount}
                totalAmount={totalAmount}
                mode={props?.mode}
                errors={errors}
                setErrors={setErrors}
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
                <h5>No Claims added</h5>
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

export default GlobalExpenseForm;
