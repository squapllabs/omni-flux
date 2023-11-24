import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/newStyles/projectStockManagement.module.scss';
import { useFormik } from 'formik';
import Input from '../../ui/Input';
import DatePicker from '../../ui/CustomDatePicker';
import { format } from 'date-fns';
import Checkbox from '../../ui/Checkbox';
import Button from '../../ui/Button';
import { useCreateStockAudit } from '../../../hooks/stockAudit-hooks';
import CustomSnackBar from '../../ui/customSnackBar';
import StockAuditService from '../../../service/stockaudit-service';
/* Stock data add for project */
const ProjectStockAdd: React.FC = (props: any) => {
  /* Function to change date value in a desired format */
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  const currentDate = new Date();
  const projectId = props.project_id;
  const [initialValues] = useState({
    project_id: projectId,
    site_id: props.siteId,
    stock_audit_date: dateFormat(currentDate),
  });
  const [itemsList, setItemsList] = useState<any>([]);
  const [itemData, setItemData] = useState<any>([]);
  const [check, setChecked] = useState(false);
  const [message] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const { mutate: postStockData } = useCreateStockAudit();
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const updateQuantity = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: number
  ) => {
    const updatedArray = [...itemData];
    updatedArray[index].quantity = event.target.value;
    setItemsList(updatedArray);
  };
  /* Function to create stock details */
  const formik = useFormik({
    initialValues,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      const obj: any = {
        ...values,
        item_details: itemsList,
        created_by: 1,
      };
      postStockData(obj, {
        onSuccess(data, variables, context) {
          if (data?.message === 'success') {
            resetForm();
            props.setMessage('Stock audited successfully');
            props.setOpenSnack(true);
            props.setOpen(false);
            props.setModalOpen(false);
            props.setReload(true);
          }
        },
      });
    },
  });
  const handleClose = () => {
    props.setOpen(false);
    props.setModalOpen(false);
  };
  /* Function to get item data  */
  useEffect(() => {
    const searchCategory = async () => {
      const values = {
        projectId: projectId,
        siteId: props.siteId,
      };
      try {
        const itemsData = await StockAuditService.getItems(values);
        const finalData = itemsData?.data;
        const initialArray = finalData?.map((item: any) => ({
          item_name: item?.item_data?.item_name,
          quantity: '',
        }));
        setItemData(initialArray);
      } catch (err) {
        console.log('error in list : ', err);
      }
    };
    searchCategory();
  }, [props?.siteId, projectId]);

  return (
    <div className={Styles.stock_Container}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.divOne}>
          <div style={{ width: '30%' }}>
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
                  formik.setFieldValue(
                    'stock_audit_date',
                    dateFormat(new Date())
                  );
                }}
              />
              <span style={{ fontSize: 'small' }}> Edit stock date</span>
            </div>
          </div>
        </div>
        <div>
          <div className={Styles.tableContainer}>
            <table className={Styles.scrollable_table}>
              <thead>
                <tr>
                  <th className={Styles.tableHeading}>#</th>
                  <th className={Styles.tableHeading}>Item</th>
                  <th className={Styles.tableHeading}>Quantity</th>
                </tr>
              </thead>
              <tbody>
                {itemData && itemData.length > 0 ? (
                  itemData.map((item: any, index: any) => (
                    <tr key={index}>
                      <td>{index + 1}</td>
                      <td>
                        <Input
                          width="50%"
                          name="item_name"
                          value={item.item_name}
                          readOnly
                        />
                      </td>
                      <td>
                        <Input
                          width="30%"
                          name="quantity"
                          onChange={(e) => updateQuantity(e, index)}
                        />
                      </td>
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td colSpan="4" style={{ textAlign: 'center' }}>
                      No data found
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
          <div className={Styles.footer}>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.button}>
              <Button
                shape="rectangle"
                justify="center"
                size="small"
                onClick={handleClose}
                color='cancel'
                // className={Styles.cancelButton}
              >
                Cancel
              </Button>
              <Button
                shape="rectangle"
                color="primary"
                justify="center"
                size="small"
                type="submit"
              >
                Save
              </Button>
            </div>
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
