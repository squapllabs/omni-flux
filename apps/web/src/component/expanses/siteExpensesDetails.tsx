import React, { useState } from 'react';
import Styles from '../../styles/expanses.module.scss';
import Input from '../ui/Input';
import Button from '../ui/Button';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetBymasertDataTypeDrop } from '../../hooks/masertData-hook';
import * as Yup from 'yup';
import { useFormik } from 'formik';
import Checkbox from '../ui/Checkbox';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomDelete from '../ui/customDeleteDialogBox';
import AddIcon from '../menu/icons/addIcon';

const SiteExpensesDetails: React.FC = (props: any) => {

  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    expense_data_id: '',
    site_expense_name: '',
    bill_number: '',
    total: '',
    is_delete: false,
  });
  const [ExpenseValue, setExpenseValue] = useState<any>({});
  const [checked, setChecked] = useState(false);
  const [openDelete, setOpenDelete] = useState(false);
  const { data: getSiteExpense } = useGetBymasertDataTypeDrop('SIEP');
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };

  const deleteSiteExpense = (e: any, values: any) => {
    const itemIndex = props.expenseList.findIndex(
      (item: any) =>
        item.expense_data_id === ExpenseValue?.expense_data_id &&
        item.is_delete === ExpenseValue?.is_delete
    );

    props.expenseList[itemIndex] = {
      ...props.expenseList[itemIndex],
      is_delete: true,
    };
    props.setExpenseList([...props.expenseList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
    props.setMessage('Site expanse details has been deleted');
    props.setOpenSnack(true);
  };
  const validationSchema = Yup.object().shape({
    is_delete: Yup.string().required(),
    expense_data_id: Yup.string()
      .typeError('Site Expense is required')
      .required('Site Expense is required')
      .test(
        'description-availability',
        'Site Expense is already present',
        async function (value, { parent }: Yup.TestContext) {
          const isDelete = parent.is_delete;
          try {
            const isValuePresent = props.expenseList.some((obj) => {
              return (
                obj.site_expense === Number(value) && obj.is_delete === isDelete
              );
            });
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
    total: Yup.number()
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
      values['total'] = Number(values.total);
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
            <div style={{ width: '30%' }}>
              <AutoCompleteSelect
                name="expense_data_id"
                label="Site Expense"
                defaultLabel="Select from options"
                placeholder="Select from options"
                onChange={formik.handleChange}
                value={formik.values.expense_data_id}
                optionList={getSiteExpense}
                onSelect={(value) => {
                  formik.setFieldValue('expense_data_id', value);
                  const matchingObjects = getSiteExpense.filter(
                    (obj: any) => Number(obj.value) === Number(value)
                  );
                  formik.setFieldValue(
                    'site_expense_name',
                    matchingObjects[0].label
                  );
                }}
                error={
                  formik.touched.expense_data_id &&
                  formik.errors.expense_data_id
                }
              />
            </div>

            <div style={{ width: '27%', paddingTop: '20px' }}>
              <Input
                name="bill_number"
                label="Bill No"
                value={formik.values.bill_number}
                onChange={formik.handleChange}
                disabled={checked === true ? false : true}
              />
              <div
                style={{ display: 'flex', gap: '10px', alignItems: 'center' }}
              >
                <Checkbox
                  checked={checked}
                  onChange={() => {
                    setChecked(!checked);
                  }}
                />
                <span style={{ fontSize: '70%' }}>Is bill available ?</span>
              </div>
            </div>
            <div style={{ width: '25%' }}>
              <Input
                name="total"
                label="Amount"
                value={formik.values.total}
                onChange={formik.handleChange}
                error={formik.touched.total && formik.errors.total}
              />
            </div>
            <div style={{ width: '10%' }}>
              <Button
                type="button"
                color="primary"
                shape="rectangle"
                size="small"
                justify="center"
                icon={<AddIcon color="white" />}
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
            <th>#</th>
            <th>Site Expense</th>
            <th>Bill No</th>
            <th>Amount</th>
            <th>Action</th>
          </thead>
          <tbody>
            {props.expenseList?.length === 0 ? (
              <tr>
                <td colSpan="5" style={{ textAlign: 'center' }}>
                  No data found
                </td>
              </tr>
            ) : (
              ''
            )}
            {props.expenseList?.map((item: any, index: any) => {
              if (item.is_delete === false) {
                rowIndex = rowIndex + 1;
                return (
                  <tr>
                    <td>{rowIndex}</td>
                    <td>
                      {item?.site_expense_name === undefined
                        ? item?.expense_master_data?.master_data_name
                        : item?.site_expense_name}
                    </td>
                    <td>{item.bill_number}</td>
                    <td>{item.total}</td>
                    <td>
                      <div
                        style={{ cursor: 'pointer' }}
                        onClick={() => {
                          setExpenseValue(item);
                          setOpenDelete(true);
                        }}
                      >
                        <DeleteIcon />
                      </div>
                    </td>
                  </tr>
                );
              }
            })}
          </tbody>
        </table>
      </div>
      <CustomDelete
        open={openDelete}
        title="Delete Site Expense"
        contentLine1="Are you sure you want to delete this Expense ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteSiteExpense}
      />
    </div>
  );
};

export default SiteExpensesDetails;
