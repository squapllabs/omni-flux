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
import CustomConfirmDialogBox from '../../../ui/CustomConfirmDialogBox';
import CustomDialogBox from '../../../ui/CustomDialog';
import siteExpenseService from '../../../../service/expense-service';
import CustomSnackbar from '../../../ui/customSnackBar';

const ProjectSiteExpenseForm: React.FC = (props: any) => {
  console.log('filterValue', props.siteId);

  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const projectId = Number(props?.projectId);
  const siteId = Number(props?.siteId);
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
    site_id: siteId,
    expense_id: '',
    submitType: '',
    total_amount: '',
  });
  const [expenseList, setExpenseList] = useState<any>([]);
  const validationSchema = getCreateValidateyup(Yup);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [openConfirm, setOpenConfirm] = useState(false);
  const [openDialog, setOpenDialog] = useState(false);
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
      console.log('datas', datas);
      const arry: any = [];
      // setExpenseBill(
      //   datas?.data?.bill_details === null ? [] : datas?.data?.bill_details
      // );
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
        total_amount: datas?.data?.total_amount,
      });
    };
    if (props?.mode === 'Edit') fetchData();
  }, [props?.expenseID, props?.mode]);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      console.log('values', values);

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
            bill_details: '',
            status: statusData,
            total_amount: totalSelectedPrice,
          };
          console.log('objectpost', object);
          postSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              console.log('data', data);
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
          console.log('sumOfRates', totalSelectedPrice);

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
            bill_details: '',
            status: statusData,
            total_amount: totalSelectedPrice,
          };
          console.log('Editobject', object);

          updateSiteExpenseData(object, {
            onSuccess(data, variables, context) {
              if (data?.status === true) {
                console.log('editData', data);
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
      <div className={Styles.container}>
        <div className={Styles.formContainer}>
          <div className={Styles.form_fields}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
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
              </div>
              <div className={Styles.fieldStyle}>
                <Input
                  name="total_amount"
                  label="Total"
                  value={formik.values.total_amount}
                  onChange={formik.handleChange}
                  disabled
                  mandatory
                />
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
          <div>
            <FlagIcon width={50} height={50} />
          </div>
        </div>
        <div className={Styles.tableContainer}>
          <SiteExpensesDetails
            setExpenseList={setExpenseList}
            expenseList={expenseList}
            setMessage={setMessage}
            setOpenSnack={setOpenSnack}
          />
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
                // onClick={() => cancelhandler()}
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
        title="are you sure you want to save?"
        contentLine1="after saving you cant edit  "
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
    </div>
  );
};

export default ProjectSiteExpenseForm;
