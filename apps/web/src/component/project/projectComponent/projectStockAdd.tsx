import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/newStyles/projectStockManagement.module.scss';
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
import CustomSnackBar from '../../ui/customSnackBar';
// import { getProjectStockAuditValidate } from '../../../helper/constants/project-constants';
import StockAuditService from '../../../service/stockaudit-service';

const ProjectStockAdd: React.FC = (props: any) => {
  // const validationSchema = getProjectStockAuditValidate(yup);
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };

  const currentDate = new Date();
  const projectId = props.project_id;
  const [initialValues, setInitialValues] = useState({
    project_id: projectId,
    site_id: props.siteId,
    stock_audit_date: dateFormat(currentDate),
  });
  const [itemsList, setItemsList] = useState<any>([]);
  const [itemData, setItemData] = useState<any>([]);
  const [check, setChecked] = useState(false);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const { data: getSiteList } = getProjectSite(projectId);
  const { data: getProjectData } = getByProjectId(projectId);

  const { mutate: postStockData, isLoading: stockpostLoading } =
    createStockAudit();
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

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

  const updateQuantity = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: number
  ) => {
    const updatedArray = [...itemData];
    updatedArray[index].quantity = event.target.value;
    setItemsList(updatedArray);
  };

  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      const obj: any = {
        ...values,
        item_details: itemsList,
        created_by: 1,
      };
      postStockData(obj, {
        onSuccess(data, variables, context) {
          if(data?.message === 'success'){
          resetForm();
          props.setMessage('Stock audited successfully');
          props.setOpenSnack(true);
          props.setOpen(false);
          props.setReload(true);
          }
        },
      });
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  };

  useEffect(() => {
    searchCategory();
  }, [props?.siteId]);

  return (
    <div className={Styles.stock_Container}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.divOne}>
          {/* <div>
            <AutoCompleteSelect
              name="site_id"
              label="Site"
              mandatory={true}
              optionList={getSiteList}
              value={formik.values.site_id}
              onChange={formik.handleChange}
              onSelect={(value) => {
                formik.setFieldValue('site_id', value);
                if(value !== '')
                searchCategory(value); 
              }}
              width='185px'
              error={formik.touched.site_id && formik.errors.site_id}
            />
          </div> */}
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
        <div >
          {/* <ItemsTable
            setItemsList={setItemsList}
            itemsList={itemsList}
            itemData={itemData}
          /> */}
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
                className={Styles.cancelButton}
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

// const ItemsTable: React.FC = (props: any) => {
//   let rowIndex = 0;
//   const [initialValues, setInitialValues] = useState({
//     item: '',
//     quantity: '',
//   });
//   console.log('props table==>', props.itemData);
//   const initialArray = props.itemData.map((item: any) => ({
//     item_name: item?.item_data?.item_name,
//     quantity: '',
//   }));
//   const [arrayData,setArrayData] = useState<any>(initialArray)
//   console.log('initialArray', initialArray);
//   // console.log('arrayData', arrayData);
//   const validationSchema = yup.object().shape({
//     item: yup.string().required('Item is required'),
//     quantity: yup.string().required('Quantity is required'),
//   });

//   // const updateQuantity = ( event: React.ChangeEvent<HTMLInputElement>,index: number) => {
//   //   console.log("eeeeeeeeee",event);
//   //   console.log("eeeeeeeindex",index);
//   //   let tempObj: any = {};
//   //   tempObj = {
//   //         ...newArray[index],
//   //         [event.target.name]: event.target.value,
//   //       };
//   //     const tempAry = [...newArray];
//   //     tempAry[index] = tempObj;
//   //     console.log('new form data====>', tempAry);
//   // };

//   const updateQuantity = (event: React.ChangeEvent<HTMLInputElement>, index: number) => {
//     const updatedArray = [...initialArray];
//     console.log("updatedArray 1st",updatedArray);
//     console.log("event.target.value",event.target.value);
//     updatedArray[index].quantity = event.target.value;
//     console.log("updatedArray 2nd",updatedArray);
//     // setArrayData(updatedArray)
//     // console.log("arrayData",arrayData);
//   };
//   const formik = useFormik({
//     initialValues,
//     validationSchema,
//     enableReinitialize: true,
//     onSubmit: async (values, { resetForm }) => {
//       props.setItemsList([...props.itemsList, values]);
//       resetForm();
//     },
//   });
//   return (
//     <div>
//       <form>
//         <table className={Styles.scrollable_table}>
//           <thead>
//             <tr>
//               <th>S.No</th>
//               <th>Item</th>
//               <th>Quantity</th>
//             </tr>
//           </thead>
//           <tbody>
//             {initialArray.map((item : any, index : any) => (
//               <tr key={index}>
//                 <td>{index + 1}</td>
//                 <td>
//                   <Input
//                     width="50%"
//                     name="item_name"
//                     value={item.item_name}
//                     readOnly
//                   />
//                 </td>
//                 <td>
//                   <Input
//                     width="50%"
//                     name="quantity"
//                     onChange={(e) => updateQuantity(e, index)}
//                     // value={item.quantity}
//                   />
//                 </td>
//               </tr>
//             ))}
//           </tbody>
//         </table>
//       </form>
//     </div>
//   );
// };
