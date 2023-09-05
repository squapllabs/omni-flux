import React, { useEffect, useState } from 'react';
import { useGetAllItemsDrops } from '../../hooks/item-hooks';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import Styles from '../../styles/bom.module.scss';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Input from '../ui/Input';
import AddIcon from '../menu/icons/addIcon';
import { useFormik } from 'formik';
import DeleteIcon from '../menu/icons/deleteIcon';
import CustomGroupButton from '../ui/CustomGroupButton';
import Button from '../ui/Button';
import CustomDelete from '../ui/customDeleteDialogBox';
import CustomSnackBar from '../ui/customSnackBar';
import { createBulkBom } from '../../hooks/bom-hooks';
import BomService from '../../service/bom-service';
import {
  getBombulkValidateyup,
  bomErrorMessages,
} from '../../helper/constants/bom-constants';
import * as Yup from 'yup';
import { useNavigate, useParams } from 'react-router-dom';
import { getBySubcategoryID } from '../../hooks/subCategory-hooks';
import { formatBudgetValue } from '../../helper/common-function';

const Bom: React.FC = (props: any) => {
  const params = useParams();
  const navigate = useNavigate();
  const fieldWidth = '140px';
  let rowIndex = 0;
  const [bomList, setBomList] = useState<any>([]);
  const validationSchema = Yup.object().shape({
    bom_name: Yup.string().trim().required(bomErrorMessages.ENTER_NAME),
    quantity: Yup.number()
      .required(bomErrorMessages.ENTER_QUANTITY)
      .typeError(bomErrorMessages.TYPECHECK),
    item_id: Yup.string()
      .trim()
      .required(bomErrorMessages.ENTER_ITEM)
      .test(
        'decimal-validation',
        bomErrorMessages.ITEM_EXIST,
        async function (value: number, { parent }: Yup.TestContext) {
          console.log('value', value);
          console.log('parent.is_delete', parent.is_delete);
          console.log('bomList', bomList);
          let isDelete = parent.is_delete;
          try {
            const isValuePresent = bomList.some((obj: any) => {
              return (
                Number(obj.item_id) === Number(value) &&
                obj.is_delete === isDelete
              );
            });
            console.log('state', isValuePresent);
            if (isValuePresent === false) {
              return true;
            } else return false;
          } catch {
            return true;
          }
        }
      ),
    uom_id: Yup.string().trim().required(bomErrorMessages.ENTER_UOM),
  });
  const intialBom: any = {
    created_by: 1,
    sub_category_id: Number(params?.subCategoryId),
    item_id: '',
    bom_name: '',
    description: '',
    uom_id: '',
    uom_name: '',
    quantity: '',
    rate: '',
    total: 0,
    is_delete: false,
    bom_type: '',
    bom_id: '',
  };
  const [initialValues, setInitialValues] = useState(intialBom);
  const [buttonLabels, setButtonLabels] = useState([
    { label: 'RAW MATERIAL', value: 'RAWMT' },
    { label: 'LABOUR', value: 'RAWLB' },
    { label: 'MACHINERY', value: 'MAC' },
  ]);
  const [activeButton, setActiveButton] = useState<string | null>('RAWMT');
  const [bomValue, setBomValue] = useState();
  const [openDelete, setOpenDelete] = useState(false);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const { data: getAllItemDrop } = useGetAllItemsDrops();
  const { data: getAllUomDrop } = useGetAllUomDrop();
  const { mutate: bulkBomData, data: responseData } = createBulkBom();
  const { data: getSubCategoryData } = getBySubcategoryID(
    Number(params?.subCategoryId)
  );
  console.log('getSubCategoryData', getSubCategoryData);

  useEffect(() => {
    const fetchData = async () => {
      const obj = {
        id: params?.subCategoryId,
        type: activeButton,
      };
      const getData = await BomService.getBOMbySubCatIDandType(obj);
      console.log('getData', getData?.data);
      console.log('getData', getData);
      if (getData?.status === true) setBomList(getData?.data);
    };
    fetchData();
  }, [activeButton]);
  const handleGroupButtonClick = (value: string) => {
    setActiveButton(value);
  };
  const handleDeleteSiteExpense = (e: any, value: any) => {
    setBomValue(value);
    setOpenDelete(true);
  };
  const handleCloseDelete = () => {
    setOpenDelete(false);
  };
  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    console.log('bomData', bomList[index]);
    let tempObj = {};
    if (
      event.target.name === 'quantity' ||
      event.target.name === 'price' ||
      event.target.name === 'rate'
    ) {
      tempObj = {
        ...bomList[index],
        [event.target.name]: Number(event.target.value),
      };
    } else {
      tempObj = {
        ...bomList[index],
        [event.target.name]: event.target.value,
      };
    }

    let tempArry = [...bomList];
    tempArry[index] = tempObj;
    setBomList(tempArry);
  };
  const sumOfRates = bomList.reduce((accumulator: any, currentItem: any) => {
    return accumulator + currentItem.total;
  }, 0);
  console.log('sumOfRates', sumOfRates);
  const deleteBOM = () => {
    const itemIndex = bomList.findIndex(
      (item: any) =>
        item.item_id === bomValue?.item_id &&
        item.is_delete === bomValue?.is_delete
    );
    bomList[itemIndex] = {
      ...bomList[itemIndex],
      is_delete: true,
    };
    setBomList([...bomList]);
    rowIndex = rowIndex - 1;
    setOpenDelete(false);
  };
  /* Function for closing the snackbar */
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      values['total'] = formik.values.quantity * formik.values.rate;
      values['is_delete'] = false;
      values['bom_type'] = activeButton;
      values['quantity'] = Number(formik.values.quantity);
      values['rate'] = Number(formik.values.rate);
      console.log('values', values);
      let arr = [];
      arr = [...bomList, values];
      setBomList(arr);
      resetForm();
    },
  });

  const handleBulkBomAdd = () => {
    console.log('bomList', bomList);

    bulkBomData(bomList, {
      onSuccess(data, variables, context) {
        console.log('data', data);
        if (data?.status === true) {
          setTimeout(() => {
            navigate(`/bomlist/${params?.projectId}`);
          }, 3000);
        }
      },
    });
  };
  return (
    <div className={Styles.bomcontainer}>
      <div className={Styles.mainheader}>
        <div>
          <div className={Styles.mainHeading}>
            <div className={Styles.mainLeftContent}>
              <h3>{getSubCategoryData?.category?.name}</h3>
              <p className={Styles.descriptionContent}>
                {getSubCategoryData?.category?.description}
              </p>
            </div>
            <div>
              <p>Allocated Budget</p>
              <p>
                {formatBudgetValue(
                  getSubCategoryData?.category?.budget
                    ? getSubCategoryData?.category?.budget
                    : 0
                )}
              </p>
            </div>
          </div>
        </div>
        <div>
          <div className={Styles.mainHeading}>
            <div className={Styles.mainLeftContent}>
              <h3>{getSubCategoryData?.name}</h3>
              <p className={Styles.descriptionContent}>
                {getSubCategoryData?.description}
              </p>
            </div>
            <div>
              <p>Allocated Budget</p>
              <p>
                {formatBudgetValue(
                  getSubCategoryData?.budget ? getSubCategoryData?.budget : 0
                )}
              </p>
            </div>
          </div>
        </div>
      </div>
      <div className={Styles.groupButton}>
        <CustomGroupButton
          labels={buttonLabels}
          onClick={handleGroupButtonClick}
          activeButton={activeButton}
        />
      </div>
      <div className={Styles.mainBody}>
        <div className={Styles.tableContainer}>
          <table className={Styles.scrollable_table}>
            <thead>
              <tr>
                <th>S No</th>
                <th>Item</th>
                {/* <th>Description</th> */}
                <th>UOM</th>
                <th>Quantity</th>
                <th>Rate</th>
                <th>Total</th>
                <th>Action</th>
              </tr>
            </thead>
            <tbody>
              {bomList?.map((items: any, index: any) => {
                if (items.is_delete === false) {
                  rowIndex = rowIndex + 1;
                  return (
                    <tr>
                      <td>{rowIndex}</td>
                      <td>{items.bom_name}</td>
                      {/* <td>
                      <Input
                        name="description"
                        width={fieldWidth}
                        value={items?.description}
                        onChange={(e) => handleListChange(e, index)}
                      />
                    </td> */}
                      <td>
                        <AutoCompleteSelect
                          width="250px"
                          name="uom_id"
                          mandatory={true}
                          optionList={getAllUomDrop}
                          value={items.uom_id}
                          onChange={(e) => handleListChange(e, index)}
                        />
                      </td>
                      <td>
                        <Input
                          width={fieldWidth}
                          name="quantity"
                          mandatory={true}
                          value={items.quantity}
                          onChange={(e) => handleListChange(e, index)}
                        />
                      </td>
                      <td>
                        <Input
                          name="rate"
                          width={fieldWidth}
                          value={items.rate}
                          onChange={(e) => handleListChange(e, index)}
                        />
                      </td>
                      <td>
                        <div
                          style={{
                            paddingBottom: '20px',
                          }}
                        >
                          <label>{items.quantity * items.rate}</label>
                        </div>
                      </td>
                      <td>
                        <div
                          style={{
                            cursor: 'pointer',
                            paddingBottom: '20px',
                          }}
                        >
                          <div
                            onClick={(e: any) =>
                              handleDeleteSiteExpense(e, items)
                            }
                          >
                            <DeleteIcon />
                          </div>
                        </div>
                      </td>
                    </tr>
                  );
                }
              })}
              <tr>
                <td>{rowIndex + 1}</td>
                <td>
                  <AutoCompleteSelect
                    width="250px"
                    name="item_id"
                    mandatory={true}
                    optionList={getAllItemDrop}
                    value={formik.values.item_id}
                    onChange={formik.handleChange}
                    error={formik.touched.item_id && formik.errors.item_id}
                    onSelect={(value) => {
                      formik.setFieldValue('item_id', value);
                      const matchingObjects = getAllItemDrop.filter(
                        (obj: any) => Number(obj.value) === Number(value)
                      );
                      formik.setFieldValue(
                        'bom_name',
                        matchingObjects[0]?.label
                      );
                      formik.setFieldValue(
                        'rate',
                        matchingObjects[0]?.temp?.rate
                      );
                    }}
                  />
                </td>
                {/* <td>
                <Input
                  name="description"
                  width={fieldWidth}
                  value={formik.values.description}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.description && formik.errors.description
                  }
                />
              </td> */}
                <td>
                  <AutoCompleteSelect
                    width="250px"
                    name="uom_id"
                    mandatory={true}
                    optionList={getAllUomDrop}
                    value={formik.values.uom_id}
                    onChange={formik.handleChange}
                    error={formik.touched.uom_id && formik.errors.uom_id}
                    onSelect={(value) => {
                      formik.setFieldValue('uom_id', value);
                    }}
                  />
                </td>
                <td>
                  <Input
                    width={fieldWidth}
                    name="quantity"
                    mandatory={true}
                    value={formik.values.quantity}
                    onChange={formik.handleChange}
                    error={formik.touched.quantity && formik.errors.quantity}
                  />
                </td>
                <td>
                  <Input
                    name="rate"
                    width={fieldWidth}
                    value={formik.values.rate}
                    onChange={formik.handleChange}
                    error={formik.touched.rate && formik.errors.rate}
                  />
                </td>
                <td>
                  <label>{formik.values.quantity * formik.values.rate}</label>
                </td>
                <td>
                  <div
                    style={{
                      cursor: 'pointer',
                      paddingBottom: '20px',
                    }}
                  >
                    <div onClick={formik.handleSubmit}>
                      <AddIcon />
                    </div>
                  </div>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
        <div className={Styles.saveButton}>
          <Button
            color="primary"
            shape="rectangle"
            justify="center"
            size="small"
            onClick={(e) => handleBulkBomAdd(e)}
          >
            SAVE
          </Button>
        </div>
      </div>

      <div className={Styles.totalPanel}>
        <div className={Styles.panelList}>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Raw Material Cost:</span>
            <span>{sumOfRates}</span>
          </div>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Manpower Cost:</span>
            <span>0</span>
          </div>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Machinery Cost:</span>
            <span>0</span>
          </div>
          <div className={Styles.panel}>
            <span className={Styles.panelTitle}>Total:</span>
            <span>{sumOfRates}</span>
          </div>
        </div>
      </div>
      <CustomDelete
        open={openDelete}
        title="Delete BOM"
        contentLine1="Are you sure you want to delete this BOM ?"
        contentLine2=""
        handleClose={handleCloseDelete}
        handleConfirm={deleteBOM}
      />
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

export default Bom;
