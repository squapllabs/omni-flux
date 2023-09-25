import React, { useEffect, useState } from 'react';
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
import {
  createStockAudit,
  getItemByProjectAndSite,
} from '../../../hooks/stockAudit-hooks';
import CheckIcon from '../../menu/icons/checkIcon';
import CustomSnackBar from '../../ui/customSnackBar';
import { getProjectStockAuditValidate } from '../../../helper/constants/project-constants';
import StockAuditService from '../../../service/stockaudit-service';
import BackArrow from '../../menu/icons/backArrow';

const ProjectStockAdd = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const validationSchema = getProjectStockAuditValidate(yup);
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    const formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };
  const currentDate = new Date();
  const projectId = Number(routeParams?.id);
  const [initialValues, setInitialValues] = useState({
    project_id: Number(routeParams?.id),
    site_id: '',
    stock_audit_date: dateFormat(currentDate),
  });
  const [itemsList, setItemsList] = useState<any>([]);
  const [itemData, setItemData] = useState<any>([]);
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

  const searchCategory = async (value: any) => {
    const values = {
      projectId: projectId,
      siteId: value,
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
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values, { resetForm }) => {
      const obj: any = {
        ...values,
        item_details: itemsList,
        created_by: 1,
      };
      postStockData(obj, {
        onSuccess(data, variables, context) {
          // resetForm();
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
    <div className={Styles.stock_Container}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.box}>
          <div className={Styles.mainTextContent}>
            <div className={Styles.textContent_1}>
              <h3>Project Name :{getProjectData?.project_name}</h3>
              <span className={Styles.content}>
                Add your Stock audit in day basis
              </span>
            </div>
            <div className={Styles.backButton}>
              <Button
                type="button"
                color="secondary"
                shape="rectangle"
                size="small"
                justify="center"
                icon={<BackArrow />}
                onClick={(e) => {
                  navigate(`/project-edit/${routeParams?.id}`);
                }}
              >
                Back
              </Button>
            </div>
            {/* <div
              style={{
                width: '70%',
                display: 'flex',
                justifyContent: 'flex-end',
              }}
            >
              <p>back button</p>
            </div> */}
          </div>
        </div>
        <div
          style={{
            display: 'flex',
            gap: '10px',
            alignItems: 'center',
            padding: '10px',
          }}
        >
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
                searchCategory(value);
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
                  formik.setFieldValue(
                    'stock_audit_date',
                    dateFormat(new Date())
                  );
                }}
              />
              <span style={{ fontSize: 'small' }}>Edit stock date</span>
            </div>
          </div>
        </div>
        <div>
          {/* <ItemsTable
            setItemsList={setItemsList}
            itemsList={itemsList}
            itemData={itemData}
          /> */}
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S.No</th>
                <th>Item</th>
                <th>Quantity</th>
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
                        width="50%"
                        name="quantity"
                        onChange={(e) => updateQuantity(e, index)}
                      />
                    </td>
                  </tr>
                ))
              ) : (
                <tr>
                  <td></td>
                  <td>No data available</td>
                  <td></td>
                </tr>
              )}
            </tbody>
          </table>
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
