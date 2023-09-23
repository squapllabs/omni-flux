import React, { useState } from 'react';
import Styles from '../../styles/expanses.module.scss';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { getBymasertDataTypeDrop } from '../../hooks/masertData-hook';
import * as Yup from 'yup';
import { useFormik } from 'formik';
import Checkbox from '../ui/Checkbox';

const SiteExpensesDetails: React.FC = (props: any) => {
  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    site_expense: '',
    site_expense_name: '',
    bill_no: '',
    amount: '',
    is_delete: 'N',
  });
  const [checked, setChecked] = useState(false);
  const { data: getSiteExpense } = getBymasertDataTypeDrop('SIEP');
  const validationSchema = Yup.object().shape({
    is_delete: Yup.string().required(),
    site_expense: Yup.string()
      .typeError('Site Expense is required')
      .required('Site Expense is required')
      .test(
        'description-availability',
        'Site Expense is already present',
        async function (value, { parent }: Yup.TestContext) {
          const isDelete = parent.is_delete;
          console.log('isDelete', isDelete);
          console.log('value', value);

          try {
            const isValuePresent = props.expenseList.some((obj) => {
              console.log('obj', obj);
              return (
                obj.site_expense === Number(value) && obj.is_delete === isDelete
              );
            });
            console.log('isValuePresent', isValuePresent);

            if (isValuePresent === true) {
              return false;
            } else {
              return true;
            }
          } catch {
            return true;
          }
        }
      ),
    amount: Yup.number()
      .min(1, 'Amount must be more then 0')
      .max(100000, 'Amount must be less then 100000')
      .typeError('Amount is required')
      .required('Amount is required'),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      console.log('values', values);
      props.setExpenseList([...props.expenseList, values]);
      setChecked(false);
      resetForm();
    },
  });
  return (
    <div>
      <form>
        <div className={Styles.fields_container}>
          <div className={Styles.siteExpenseField}>
            <div style={{ width: '25%' }}>
              <AutoCompleteSelect
                name="site_expense"
                label="Site Expense"
                defaultLabel="Select from options"
                placeholder="Select from options"
                onChange={formik.handleChange}
                value={formik.values.site_expense}
                optionList={getSiteExpense}
                onSelect={(value) => {
                  formik.setFieldValue('site_expense', value);
                  const matchingObjects = getSiteExpense.filter(
                    (obj: any) => Number(obj.value) === Number(value)
                  );
                  formik.setFieldValue(
                    'site_expense_name',
                    matchingObjects[0].label
                  );
                }}
                error={
                  formik.touched.site_expense && formik.errors.site_expense
                }
              />
            </div>
            <div>
              <Checkbox
                checked={checked}
                onChange={() => {
                  setChecked(!checked);
                }}
              />
              <span>Is bill available ?</span>
            </div>
            <div style={{ width: '25%' }}>
              <Input
                name="bill_no"
                label="Bill No"
                value={formik.values.bill_no}
                onChange={formik.handleChange}
                disabled={checked === true ? false : true}
              />
            </div>
            <div style={{ width: '20%' }}>
              <Input
                name="amount"
                label="Amount"
                value={formik.values.amount}
                onChange={formik.handleChange}
                error={formik.touched.amount && formik.errors.amount}
              />
            </div>
            <div style={{ width: '10%' }}>
              <Button
                type="button"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
                onClick={formik.handleSubmit}
              >
                Add
              </Button>
            </div>
          </div>
        </div>
      </form>
      <div className={Styles.table_container}>
        <table className={Styles.scrollable_table}>
          <thead>
            <th>SI No</th>
            <th>Site Expense</th>
            <th>Bill No</th>
            <th>Amount</th>
            <th>Action</th>
          </thead>
          <tbody>
            {props.expenseList?.map((item: any, index: any) => {
              rowIndex = rowIndex + 1;
              return (
                <tr>
                  <td>{rowIndex}</td>
                  <td>{item.site_expense_name}</td>
                  <td>{item.bill_no}</td>
                  <td>{item.amount}</td>
                  <td></td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default SiteExpensesDetails;
