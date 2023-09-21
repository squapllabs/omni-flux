import React, { useState } from 'react';
import Styles from '../../../styles/project.module.scss';
import { useFormik } from 'formik';
import * as yup from 'yup';
import Input from '../../ui/Input';
import { getProjectSite, getByProjectId } from '../../../hooks/project-hooks';
import { useNavigate, useParams } from 'react-router-dom';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import DatePicker from '../../ui/CustomDatePicker';
import { format } from 'date-fns';
import Checkbox from '../../ui/Checkbox';
import Button from '../../ui/Button';
import { createStockAudit } from '../../../hooks/stockAudit-hooks';
import CheckIcon from '../../menu/icons/checkIcon';
import CustomSnackBar from '../../ui/customSnackBar';
import { getProjectStockAuditValidate } from '../../../helper/constants/project-constants';

const ProjectStockAdd = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const validationSchema = getProjectStockAuditValidate(yup);
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  let currentDate = new Date();
  const [initialValues, setInitialValues] = useState({
    project_id: Number(routeParams?.id),
    site_id: '',
    stock_audit_date: dateFormat(currentDate),
  });
  const [itemsList, setItemsList] = useState<any>([]);
  const [check, setChecked] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const { data: getSiteList } = getProjectSite(Number(routeParams?.id));
  const { data: getProjectData } = getByProjectId(Number(routeParams?.id));
  const { mutate: postStockData, isLoading: stockpostLoading } =
    createStockAudit();
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      const obj: any = {
        ...values,
        item_details: itemsList,
      };
      postStockData(obj, {
        onSuccess(data, variables, context) {
          resetForm();
          setMessage('Stock audited successfully');
          setOpenSnack(true);
          setTimeout(() => {
            navigate(`/project-edit/${routeParams?.id}`);
          }, 2000);
        },
      });
    },
  });

  return (
    <div className={Styles.Container}>
      <form onSubmit={formik.handleSubmit}>
        <div
          style={{
            display: 'flex',
            gap: '10px',
            alignItems: 'center',
            padding: '10px',
          }}
        >
          <div style={{ width: '30%' }}>
            <Input
              label="Project Name"
              value={getProjectData?.project_name}
              disabled={true}
            />
          </div>
          <div style={{ width: '30%' }}>
            <AutoCompleteSelect
              name="site_id"
              label="Site"
              mandatory={true}
              optionList={getSiteList}
              value={formik.values.site_id}
              onChange={formik.handleChange}
              onSelect={(value) => {
                formik.setFieldValue('site_id', value);
              }}
              error={formik.touched.site_id && formik.errors.site_id}
            />
          </div>
          <div style={{ width: '30%', paddingTop: '20px' }}>
            <DatePicker
              name="stock_audit_date"
              label="Stock Date"
              value={formik.values.stock_audit_date}
              onChange={formik.handleChange}
              disabled={check === false ? true : false}
              error={
                formik.touched.stock_audit_date &&
                formik.errors.stock_audit_date
              }
            />
            <div>
              <Checkbox
                checked={check}
                onChange={() => {
                  setChecked(!check);
                }}
              />
              <span style={{ fontSize: 'small' }}>To audit for past date</span>
            </div>
          </div>
        </div>
        <div className={Styles.tableContainer}>
          <ItemsTable setItemsList={setItemsList} itemsList={itemsList} />
          <div className={Styles.buttons}>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              onClick={formik.handleSubmit}
              disabled={itemsList?.length === 0 ? true : false}
            >
              Submit
            </Button>
          </div>
        </div>
      </form>
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

export default ProjectStockAdd;

const ItemsTable: React.FC = (props: any) => {
  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    item: '',
    quantity: '',
  });
  const validationSchema = yup.object().shape({
    item: yup.string().required('Item is required'),
    quantity: yup.string().required('Quantity is required'),
  });
  const handleChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: number
  ) => {
    let tempObj: any = {};
    tempObj = {
      ...props.itemsList[index],
      [event.target.name]: event.target.value,
    };
    const tempArry = [...props.itemsList];
    tempArry[index] = tempObj;
    props.setItemsList(tempArry);
  };
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      props.setItemsList([...props.itemsList, values]);
      resetForm();
    },
  });
  return (
    <div>
      <form>
        <div className={Styles.buttons}>
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
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th>S.No</th>
              <th>Item</th>
              <th>Quantity</th>
              <th>Action</th>
            </tr>
          </thead>
          <tbody>
            {props.itemsList?.map((items: any, index: any) => {
              rowIndex = rowIndex + 1;
              return (
                <tr>
                  <td>{rowIndex}</td>
                  <td>
                    <Input
                      width="50%"
                      name="item"
                      onChange={(e) => handleChange(e, index)}
                      value={items?.item}
                      disabled
                    />
                  </td>
                  <td>
                    <Input
                      width="50%"
                      name="quantity"
                      onChange={(e) => handleChange(e, index)}
                      value={items?.quantity}
                    />
                  </td>
                  <td></td>
                </tr>
              );
            })}
            <tr>
              <td></td>
              <td>
                <Input
                  width="50%"
                  name="item"
                  onChange={formik.handleChange}
                  value={formik.values.item}
                  error={formik.touched.item && formik.errors.item}
                />
              </td>
              <td>
                <Input
                  width="50%"
                  name="quantity"
                  onChange={formik.handleChange}
                  value={formik.values.quantity}
                  error={formik.touched.quantity && formik.errors.quantity}
                />
              </td>
              <td></td>
            </tr>
          </tbody>
        </table>
      </form>
    </div>
  );
};
