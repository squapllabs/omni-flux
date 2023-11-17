import React, { useEffect, useState } from 'react';
import Styles from '../../styles/stockOutwardAdd.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import DatePicker from '../ui/CustomDatePicker';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import Checkbox from '../ui/Checkbox';
import { format } from 'date-fns';
import DeleteIcon from '../menu/icons/deleteIcon';
import ProjectService from '../../service/project-service';
import StockOutWardService from '../../service/stock-outward-service';
import {
  useGetUserDataProjectRolebased,
  useGetByProjectId,
} from '../../hooks/project-hooks';
import { useCreateStockOutWard } from '../../hooks/stock-outward';
import { useNavigate } from 'react-router-dom';
import {
  getStockOutwardCreationYupschema,
  getStockOutwardItemCreationYupschema,
} from '../../helper/constants/stockOutward-constants';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import CustomSnackBar from '../ui/customSnackBar';
import { useLocation } from 'react-router-dom';
import ProjectSubheader from '../project/projectSubheader';

const StoreOutwardAdd = () => {
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const location = useLocation();
  const projectId = location.state?.projectId;
  const userData: any = encryptedData.userData;
  const siteEngineerName: any = userData.first_name + ' ' + userData.last_name;
  const siteEngineerId: any = userData.user_id;

  const [initialValues, setInitialValues] = useState({
    site_id: '',
    // site_engineer_id: Number(siteEngineerId),
    site_engineer_id: '',
    stock_outward_date: format(new Date(), 'yyyy-MM-dd'),
  });
  const [checked, setChecked] = useState(false);
  const [siteChecked, setSiteChecked] = useState(false);
  const [stockData, setStockData] = useState<any>([]);
  const [disable, setDisable] = useState(true);
  const [siteData, setSiteData] = useState();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [open, setOpen] = useState(false);

  const navigate = useNavigate();
  const Obj: any = {
    projectID: projectId,
    role: 'Site Engineer',
  };
  const { data: getSiteEngineerData = [] } =
    useGetUserDataProjectRolebased(Obj);
  const { data: getProjectData } = useGetByProjectId(projectId);
  const { mutate: createNewStockOutWard } = useCreateStockOutWard();

  const validationSchema = getStockOutwardCreationYupschema(Yup);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const object: any = {
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
        })),
      };
      createNewStockOutWard(object, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Stock OutWard Created');
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

  const handleCheckBoxSiteChange = (e: any) => {
    const CheckboxValue = e.target.checked;
    setSiteChecked(CheckboxValue);
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
      return site;
    });
    setSiteData(arr);
  };

  useEffect(() => {
    fetchProjectSite();
  }, []);

  return (
    <div>
      <ProjectSubheader
        description="Manage your stock outward"
        navigation={`/project-edit/${projectId}`}
        title="Stock OutWard Add"
      />
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.container}>
        <div className={Styles.box}>
          <form onSubmit={formik.handleSubmit}>
            <div className={Styles.fields_container}>
              <div className={Styles.fields_container_1}>
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
                    value={formik.values.site_id}
                    placeholder="Select from options"
                    mandatory
                    width="350px"
                    onSelect={(value) => {
                      formik.setFieldValue('site_id', value);
                    }}
                    optionList={siteData}
                    error={formik.touched.site_id && formik.errors.site_id}
                  />
                </div>
                <div>
                  <AutoCompleteSelect
                    label="Site Engineer Name"
                    name="site_engineer_id"
                    onChange={formik.handleChange}
                    value={formik.values.site_engineer_id}
                    placeholder="Select from options"
                    mandatory
                    width="350px"
                    onSelect={(value) => {
                      formik.setFieldValue('site_engineer_id', value);
                    }}
                    optionList={getSiteEngineerData}
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
                    value={formik.values.stock_outward_date}
                  />
                </div>
              </div>
              <div className={Styles.fields_container_3}>
                <div>
                  <Checkbox
                    name="is_editable"
                    checked={checked}
                    onChange={(e) => handleCheckBoxChange(e)}
                  />
                  <span className={Styles.checkBox}> Edit OutWard Date</span>
                </div>
              </div>
              <div className={Styles.dividerStyle1}></div>
              {formik.values.site_id && (
                <div className={Styles.tableContainer}>
                  <ItemDetailsTable
                    stockData={stockData}
                    setStockData={setStockData}
                    projectId={projectId}
                  />
                </div>
              )}
            </div>
            <div className={Styles.buttonFields}>
              <div>
                <Button
                  color="primary"
                  type="button"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  disabled={stockData.length === 0 ? true : false}
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

export default StoreOutwardAdd;

const ItemDetailsTable: React.FC = (props: {
  stockData: any;
  setStockData: any;
  projectId: any;
}) => {
  const { stockData, setStockData, projectId } = props;

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

  useEffect(() => {
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
          <div>
            <Button
              type="button"
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
              onClick={() => formik.handleSubmit()}
              icon={<AddIcon color="white" />}
            >
              Add
            </Button>
          </div>
        </div>
        <table className={Styles.scrollable_table}>
          <thead>
            <tr>
              <th className={Styles.tableHeading}>#</th>
              <th className={Styles.tableHeading}>ITEM</th>
              <th className={Styles.tableHeading}>QUANTITY</th>
              <th className={Styles.tableHeading}>IN STOCK</th>
              <th className={Styles.tableHeading}>UOM</th>
              <th className={Styles.tableHeading}>Action</th>
            </tr>
          </thead>
          <tbody>
            {stockData?.map((items: any, index: any) => {
              rowIndex = rowIndex + 1;
              return (
                <tr>
                  <td>{rowIndex}</td>
                  <td>{items.item_name}</td>
                  <td>{items.outward_quantity}</td>
                  <td>{items.available_quantity}</td>
                  <td>{items.uom_name}</td>
                  <td>
                    <div
                      style={{
                        cursor: 'pointer',
                      }}
                    >
                      <div>
                        <DeleteIcon onClick={() => handleDelete(index)} />
                      </div>
                    </div>
                  </td>
                </tr>
              );
            })}
            <tr>
              <td>{rowIndex + 1}</td>
              <td>
                <AutoCompleteSelect
                  placeholder="Select from options"
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
                />
              </td>
              <td>
                <div
                  style={{
                    cursor: 'pointer',
                  }}
                ></div>
              </td>
            </tr>
          </tbody>
        </table>
      </form>
    </div>
  );
};
