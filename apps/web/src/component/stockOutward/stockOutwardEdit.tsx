import React, { useEffect, useState } from 'react';
import Styles from '../../styles/stockOutwardAdd.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import Checkbox from '../ui/Checkbox';
import { format } from 'date-fns';
import DeleteIcon from '../menu/icons/deleteIcon';
import ProjectService from '../../service/project-service';
import StockOutWardService from '../../service/stock-outward-service';
import {
  useGetUserDataProjectRolebased,
  useGetByProjectId,
} from '../../hooks/project-hooks';
import {
  useUpdateStockOutWard,
  // useGetByStockOutWardId,
} from '../../hooks/stock-outward';
import { useNavigate } from 'react-router-dom';
import {
  getStockOutwardCreationYupschema,
  getStockOutwardItemCreationYupschema,
} from '../../helper/constants/stockOutward-constants';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import CustomSnackBar from '../ui/customSnackBar';
import { useParams } from 'react-router-dom';
import { useLocation } from 'react-router-dom';
import ProjectSubheader from '../project/projectSubheader';
import NewAddCircleIcon from '../menu/icons/newAddCircleIcon';

const StoreOutwardEdit = () => {
  const routeParams = useParams();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userData: any = encryptedData?.userData;
  // const siteEngineerName: any =
  //   userData?.first_name + ' ' + userData?.last_name;
  const siteEngineerId: any = userData?.user_id;
  const location = useLocation();
  const projectId = location.state?.projectId;

  const fetchData = async () => {
    const stockOutWardData = await StockOutWardService.getOneStockOutWardId(
      Number(routeParams?.id)
    );
    const stockDate = stockOutWardData?.data?.stock_outward_date;
    let formattedDate = '';
    if (stockDate) {
      const currentDate = new Date(stockDate);
      formattedDate = format(currentDate, 'yyyy-MM-dd');
    }
    setInitialValues({
      site_id: stockOutWardData?.data?.site_id || '',
      outward_id: stockOutWardData?.data?.outward_id || '',
      // site_engineer_id: Number(siteEngineerId),
      site_engineer_id: stockOutWardData?.data?.site_engineer_id || '',
      stock_outward_date: formattedDate || format(new Date(), 'yyyy-MM-dd'),
      stock_outward_id: stockOutWardData?.data?.stock_outward_id,
    });
  };

  const fetchProjectSite = async () => {
    const siteData = await ProjectService.getOneProjectSite(projectId);
    const arr: any = [];
    const siteValues = siteData?.data?.map((site: any) => {
      const obj: any = {
        value: site?.site_id,
        label: site?.site_details?.name,
      };
      arr.push(obj);
      return obj;
    });
    setSiteData(arr);
  };

  const [initialValues, setInitialValues] = useState({
    site_id: '',
    outward_id: '',
    // site_engineer_id: Number(siteEngineerId),
    site_engineer_id: '',
    stock_outward_date: format(new Date(), 'yyyy-MM-dd'),
    stock_outward_id: '',
  });
  const [checked, setChecked] = useState(false);
  const [siteChecked, setSiteChecked] = useState(false);
  const [stockData, setStockData] = useState<any>([]);
  const [disable, setDisable] = useState(true);
  const [siteData, setSiteData] = useState<any>([]);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [open, setOpen] = useState(false);

  useEffect(() => {
    fetchProjectSite();
    fetchData();
  }, []);

  const navigate = useNavigate();
  const Obj: any = {
    projectID: projectId,
    role: 'Site Engineer',
  };

  const { data: getSiteEngineerData } = useGetUserDataProjectRolebased(Obj);
  const { data: getProjectData } = useGetByProjectId(projectId);
  const { mutate: updateOneStockOutWard } = useUpdateStockOutWard();
  const validationSchema = getStockOutwardCreationYupschema(Yup);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const object: any = {
        stock_outward_id: values.stock_outward_id,
        project_id: Number(projectId),
        site_id: values.site_id,
        site_engineer_id: values.site_engineer_id,
        item_count: stockData?.length,
        stock_outward_date: values.stock_outward_date,
        created_by: Number(siteEngineerId),
        stock_outward_details: stockData.map((item: any) => ({
          item_id: item.item_id,
          outward_quantity: Number(item.outward_quantity),
          uom_id: item.uom_id,
          is_delete: false,
          stock_outward_details_id: item?.stock_outward_details_id || '',
        })),
      };

      updateOneStockOutWard(object, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Stock OutWard Updated');
            setOpenSnack(true);
            setTimeout(() => {
              navigate(`/project-edit/${projectId}`);
            }, 1000);
            resetForm();
          }
        },
      });
    },
  });

  const handleCheckBoxChange = (e: any) => {
    const CheckboxValue = e.target.checked;
    setChecked(CheckboxValue);
    setDisable(!disable);
  };

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  // const handleCheckBoxSiteChange = (e: any) => {
  //   const CheckboxValue = e.target.checked;
  //   setSiteChecked(CheckboxValue);
  // };

  return (
    <div>
      <ProjectSubheader
        description="Manage your stock outward"
        navigation={`/project-edit/${projectId}`}
        title="Stock OutWard Edit"
      />
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.container}>
        <div className={Styles.box}>
          <form onSubmit={formik.handleSubmit}>
            <div className={Styles.fields_container}>
              <div className={Styles.fields_container_1}>
                {/* <div>
                  <Input
                    label="OutWardID"
                    // placeholder="STO-YYYY-"
                    name="outward_id"
                    // mandatory={true}
                    disabled={true}
                    width="350px"
                    value={formik.values?.outward_id}
                    onChange={formik.handleChange}
                    // error={
                    //     formik.touched.quantity && formik.errors.quantity
                    // }
                  />
                </div> */}
                {/* <div>
                  <Input
                    label="Project"
                    // name="user_id"
                    width="350px"
                    // onSelect={(value) => {
                    //     formik.setFieldValue('user_id', value);
                    // }}
                    value={getProjectData?.project_name}
                    disabled={true}
                    // error={
                    //     formik.touched.user_id &&
                    //     formik.errors.user_id
                    // }
                  />
                </div> */}
                <div className={Styles.topHeading}>
                  <span className={Styles.heading}>Outward ID</span>
                  <h3>{initialValues?.outward_id}</h3>
                </div>
                <div className={Styles.lineStyles}>
                  <div className={Styles.vertical}>
                    <div className={Styles.verticalLine}></div>
                  </div>
                </div>
                <div className={Styles.topHeading}>
                  <span className={Styles.heading}>Project Name</span>
                  <h3>{getProjectData?.project_name}</h3>
                </div>
              </div>
              <div className={Styles.fields_container_2}>
                <div>
                  <AutoCompleteSelect
                    label="Site"
                    name="site_id"
                    onChange={formik.handleChange}
                    value={formik.values?.site_id}
                    placeholder="Select from options"
                    mandatory
                    width="350px"
                    onSelect={(value) => {
                      formik.setFieldValue('site_id', value);
                    }}
                    optionList={siteData !== undefined ? siteData : []}
                    error={formik.touched.site_id && formik.errors.site_id}
                  />
                </div>
                <div>
                  <AutoCompleteSelect
                    label="Site Engineer Name"
                    name="site_engineer_id"
                    onChange={formik.handleChange}
                    value={formik.values?.site_engineer_id}
                    placeholder="Select from options"
                    mandatory
                    // disabled={siteChecked === false ? true : false}
                    width="350px"
                    onSelect={(value) => {
                      formik.setFieldValue('site_engineer_id', value);
                    }}
                    optionList={
                      getSiteEngineerData !== undefined
                        ? getSiteEngineerData
                        : []
                    }
                    error={
                      formik.touched.site_engineer_id &&
                      formik.errors.site_engineer_id
                    }
                  />
                </div>
                <div>
                  <DatePicker
                    label="OutWard Date"
                    name="stock_outward_date"
                    onChange={formik.handleChange}
                    mandatory
                    disabled={disable}
                    width="350px"
                    value={formik.values?.stock_outward_date}
                    // error={formik.touched.access_start_date && formik.errors.access_start_date}
                  />
                </div>
              </div>
              <div className={Styles.fields_container_3}>
                <div>
                  <Checkbox
                    name="is_editable"
                    checked={checked}
                    onChange={(e) => handleCheckBoxChange(e)}
                    // label="Edit OutWard Date"
                  />
                  <span className={Styles.checkBox}> Edit OutWard Date</span>
                </div>
              </div>
              <div className={Styles.dividerStyle1}></div>
              <div className={Styles.tableContainer}>
                <ItemDetailsTable
                  stockData={stockData}
                  setStockData={setStockData}
                  stockOutWardId={routeParams?.id}
                  projectId={projectId}
                />
              </div>
            </div>
            <div className={Styles.buttonFields}>
              <div>
                <Button
                  color="primary"
                  type="button"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  disabled={stockData?.length === 0 ? true : false}
                  onClick={() => formik.handleSubmit()}
                >
                  Save
                </Button>
              </div>
            </div>
          </form>
        </div>
      </div>
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
export default StoreOutwardEdit;

const ItemDetailsTable: React.FC = (props: {
  stockData: any;
  setStockData: any;
  stockOutWardId: any;
  projectId: any;
}) => {
  const { stockData, setStockData, stockOutWardId, projectId } = props;

  let rowIndex = 0;
  const [initialValues, setInitialValues] = useState({
    item_id: '',
    item_name: '',
    outward_quantity: '',
    available_quantity: '',
    uom_name: '',
    uom_id: '',
  });
  const [itemData, setItemData] = useState<any>([]);
  const [itemDetails, setItemDetails] = useState();
  const [display, setDisplay] = useState(false);
  const validationSchema = getStockOutwardItemCreationYupschema(Yup);

  const fetchProjectInventoryItem = async () => {
    const itemData = await StockOutWardService.getProjectInventoryItem(
      projectId
    );
    setItemDetails(itemData?.data);
    const arr: any = [];
    const itemValues = itemData?.data?.map((item: any) => {
      const obj: any = {
        value: item?.item_id,
        label: item?.item_data?.item_name,
      };
      arr.push(obj);
      return obj;
    });
    setItemData(arr);
  };

  const fetchData = async () => {
    const stockOutWardData = await StockOutWardService.getOneStockOutWardId(
      Number(stockOutWardId)
    );
    const transformedStockOutwardDetails =
      stockOutWardData?.data?.stock_outward_details?.map((item: any) => ({
        item_id: item?.item_id,
        item_name: item?.item_data?.item_name,
        outward_quantity: Number(item?.outward_quantity),
        uom_id: item?.uom_id,
        uom_name: item?.uom_data?.name,
        stock_outward_details_id: item?.stock_outward_details_id,
      }));
    setStockData(transformedStockOutwardDetails);
  };

  useEffect(() => {
    fetchData();
    fetchProjectInventoryItem();
  }, []);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      setStockData([...stockData, values]);
      resetForm();
    },
  });

  const handleDelete = (index: number) => {
    stockData.splice(index, 1);
    setStockData([...stockData]);
  };

  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.secondHeader}>
          <div>
            <h3>Item Details</h3>
          </div>
          {display && (
            <div>
              {/* <Button
                type="button"
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
               
                icon={<AddIcon color="white" />}
              >
                Add
              </Button> */}
              <div
                onClick={() => formik.handleSubmit()}
                className={Styles.iconContent}
              >
                <NewAddCircleIcon />
                <span>Add Item </span>
              </div>
            </div>
          )}
          {display === false && (
            <div
              onClick={() => {
                setDisplay(true);
              }}
              className={Styles.iconContent}
            >
              <NewAddCircleIcon />
              <span>Add Item Row</span>
            </div>
          )}
        </div>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th>S No</th>
              <th>ITEM</th>
              <th>QUANTITY</th>
              <th>IN STOCK</th>
              <th>UOM</th>
              <th>Action</th>
            </tr>
          </thead>
          <tbody>
            {stockData?.map((items: any, index: any) => {
              rowIndex = rowIndex + 1;
              return (
                <tr key={items?.stock_outward_details_id}>
                  <td>{rowIndex}</td>
                  <td>{items?.item_name}</td>
                  <td>{items?.outward_quantity}</td>
                  <td>
                    {items?.available_quantity === undefined
                      ? '-'
                      : items?.available_quantity}
                  </td>
                  <td>{items?.uom_name}</td>
                  <td>
                    {items?.stock_outward_details_id === undefined ? (
                      <div
                        style={{
                          cursor: 'pointer',
                          // paddingBottom: '20px',
                        }}
                      >
                        <div>
                          <DeleteIcon onClick={() => handleDelete(index)} />
                        </div>
                      </div>
                    ) : (
                      ''
                    )}
                  </td>

                  {/* <td>
                                        <div
                                            style={{
                                                cursor: 'pointer',
                                                // paddingBottom: '20px',
                                            }}
                                        >
                                            <div >
                                                <DeleteIcon
                                                    onClick={() => handleDelete(items?.stock_outward_details_id)}
                                                />
                                            </div>
                                        </div>
                                    </td> */}
                </tr>
              );
            })}
            {display && (
              <tr>
                <td>{rowIndex + 1}</td>
                <td>
                  <AutoCompleteSelect
                    placeholder="Select from options"
                    // width="250px"
                    name="item_id"
                    mandatory={true}
                    optionList={itemData}
                    value={formik.values.item_id}
                    onChange={formik.handleChange}
                    onSelect={(value) => {
                      formik.setFieldValue('item_id', value);
                      const matchingObjects = itemDetails?.filter(
                        (obj: any) => Number(obj.item_id) === Number(value)
                      );
                      formik.setFieldValue(
                        'available_quantity',
                        matchingObjects[0]?.available_quantity
                      );
                      formik.setFieldValue(
                        'uom_id',
                        matchingObjects[0]?.item_data?.uom_id
                      );
                      formik.setFieldValue(
                        'uom_name',
                        matchingObjects[0]?.item_data?.uom?.name
                      );
                      formik.setFieldValue(
                        'item_name',
                        matchingObjects[0]?.item_data?.item_name
                      );
                    }}
                    error={formik.touched.item_id && formik.errors.item_id}
                  />
                </td>
                <td>
                  <Input
                    name="outward_quantity"
                    value={formik.values.outward_quantity}
                    width="140px"
                    onChange={formik.handleChange}
                    error={
                      formik.touched.outward_quantity &&
                      formik.errors.outward_quantity
                    }
                  />
                </td>
                <td>
                  <Input
                    name="available_quantity"
                    width="180px"
                    value={formik.values.available_quantity}
                    onChange={formik.handleChange}
                    disabled={true}
                    error={
                      formik.touched.available_quantity &&
                      formik.errors.available_quantity
                    }
                  />
                </td>
                <td>
                  <Input
                    name="uom_name"
                    width="180px"
                    disabled={true}
                    value={formik.values.uom_name}
                    onChange={formik.handleChange}
                    // error={formik.touched.bom_name && formik.errors.bom_name}
                  />
                </td>
                <td>
                  <div
                    style={{
                      cursor: 'pointer',
                      // paddingBottom: '20px',
                    }}
                  >
                    {/* <div >
                                        <DeleteIcon onClick={() => handleDelete(rowIndex)} />
                                    </div> */}
                  </div>
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </form>
    </div>
  );
};
