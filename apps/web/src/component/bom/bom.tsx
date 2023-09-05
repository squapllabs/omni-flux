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
import { createBulkBom } from '../../hooks/bom-hooks';
import BomService from '../../service/bom-service';
import {
  getBombulkValidateyup,
  bomErrorMessages,
} from '../../helper/constants/bom-constants';
import * as Yup from 'yup';
import { useNavigate, useParams } from 'react-router-dom';

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
  const { data: getAllItemDrop } = useGetAllItemsDrops();
  const { data: getAllUomDrop } = useGetAllUomDrop();
  const { mutate: bulkBomData, data: responseData } = createBulkBom();

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
  const handleListChange = (
    event: React.ChangeEvent<HTMLInputElement>,
    index: any
  ) => {
    console.log('bomData', bomList[index]);
    let tempObj = {};
    tempObj = {
      ...bomList[index],
      [event.target.name]: event.target.value,
    };
    let tempArry = [...bomList];
    tempArry[index] = tempObj;
    setBomList(tempArry);
  };
  const sumOfRates = bomList.reduce((accumulator: any, currentItem: any) => {
    return accumulator + currentItem.total;
  }, 0);
  console.log('sumOfRates', sumOfRates);

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
    console.log('finalbomList', bomList);

    bulkBomData(bomList, {
      onSuccess(data, variables, context) {
        console.log('data', data);
        if (data?.status === true) {
          navigate(`/bomlist/${params?.projectId}`);
        }
      },
    });
  };
  return (
    <div>
      <div className={Styles.groupButton}>
        <CustomGroupButton
          labels={buttonLabels}
          onClick={handleGroupButtonClick}
          activeButton={activeButton}
        />
      </div>
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
                        <DeleteIcon />
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
                    formik.setFieldValue('bom_name', matchingObjects[0]?.label);
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
      <div className={Styles.totalPanel}>
        <div className={Styles.panelList}>
          <span>Raw Material Cost : {sumOfRates}</span>
          <span>Manpower Cost : 0</span>
          <span>Machinery Cost : 0</span>
          <span>Total : {sumOfRates}</span>
        </div>
      </div>
    </div>
  );
};

export default Bom;
