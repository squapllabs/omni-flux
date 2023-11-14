import React, { useEffect, useState } from 'react';
import Styles from '../../styles/addItem.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Button from '../ui/Button';
import { useNavigate, useParams } from 'react-router';
import CustomSnackBar from '../ui/customSnackBar';
import TextArea from '../ui/CustomTextArea';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllBrandForDrop } from '../../hooks/item-type-hooks';
import { useGetAllGstForDrop } from '../../hooks/gst-hooks';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import { useGetAllHsnForDrop } from '../../hooks/hsnCode-hooks';
import { createItem, updateItem } from '../../hooks/add-product-hooks';
import { getBymasertDataType } from '../../hooks/masertData-hook';
import addProduct from '../../service/add-product';
import {
  getCreateValidateyup,
  getUpdateValidateyup,
} from '../../helper/constants/item-constants';
import ProjectSubheader from '../project/projectSubheader';

const ProductAdd = () => {
  const { data: getAllItemTypeList = [] } = getBymasertDataType('IMTY');
  const { data: getAllGstList = [] } = useGetAllGstForDrop();
  const { data: getAllUomList = [] } = useGetAllUomDrop();
  const { data: getAllHsnList = [] } = useGetAllHsnForDrop();
  const { data: getAllBrandList = [] } = useGetAllBrandForDrop();
  const { mutate: createNewItem } = createItem();
  const { mutate: upateOneItem } = updateItem();
  const routeParams = useParams();
  const validationSchema =
    routeParams?.id === undefined
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const navigate = useNavigate();
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');

  const [disable, setDisable] = useState(
    routeParams?.id !== undefined ? true : false
  );

  const [initialValues, setInitialValues] = useState({
    item_id: '',
    item_name: '',
    code: '',
    description: '',
    hsn_code_id: '',
    gst_id: '',
    uom_id: '',
    item_type_id: '',
    brand_id: '',
    rate: '',
  });

  useEffect(() => {
    if (Number(routeParams?.id)) {
      const fetchOne = async () => {
        const data = await addProduct.getOneByItemID(Number(routeParams?.id));
        setInitialValues({
          item_id: data?.data?.item_id,
          item_name: data?.data?.item_name,
          code: data?.data?.code,
          description: data?.data?.description,
          hsn_code_id: data?.data?.hsn_code_id,
          gst_id: data?.data?.gst_id,
          uom_id: data?.data?.uom_id,
          item_type_id: data?.data?.item_type_id,
          brand_id: data?.data?.brand_id,
          rate: data?.data?.rate,
        });
      };
      fetchOne();
    }
  }, [routeParams?.id]);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (Number(routeParams?.id)) {
        const Object: any = {
          item_id: values.item_id,
          item_name: values.item_name,
          code: values.code,
          description: values.description,
          item_type_id: Number(values.item_type_id),
          gst_id: Number(values.gst_id),
          uom_id: Number(values.uom_id),
          hsn_code_id: Number(values.hsn_code_id),
          brand_id: Number(values.brand_id),
          rate: Number(values.rate),
        };
        upateOneItem(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Item Edited');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      } else {
        const Object: any = {
          item_name: values.item_name,
          code:values.code,
          description: values.description,
          item_type_id: Number(values.item_type_id),
          gst_id: Number(values.gst_id),
          uom_id: Number(values.uom_id),
          hsn_code_id: Number(values.hsn_code_id),
          brand_id: Number(values.brand_id),
          rate: Number(values.rate),
        };
        createNewItem(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Item Added');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      }
    },
  });

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div className={Styles.container}>
      <ProjectSubheader
        navigation={'/settings'}
        description={
          routeParams?.id === undefined
            ? 'Add your raw materials (Raw, Semi Finished & Finished).'
            : 'Edit your raw materials (Raw, Semi Finished & Finished).'
        }
        title={routeParams?.id === undefined ? 'Add Item' : 'Edit Item'}
      />
      <div className={Styles.form}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.itemField}>
            <div className={Styles.itemField1}>
              <div style={{ width: '30%' }}>
                <Input
                  label="Item Name"
                  placeholder="Enter item name"
                  name="item_name"
                  mandatory={true}
                  value={formik.values.item_name}
                  onChange={formik.handleChange}
                  disabled={disable}
                  error={formik.touched.item_name && formik.errors.item_name}
                />
              </div>
              <div style={{ width: '30%' }}>
                <Input
                  label="Item Code"
                  placeholder="Enter item code"
                  name="code"
                  mandatory={true}
                  value={formik.values.code}
                  onChange={formik.handleChange}
                  disabled={disable}
                  error={formik.touched.code && formik.errors.code}
                />
              </div>
            </div>
            <div className={Styles.itemField1}>
              <div style={{ width: '30%' }}>
                <Select
                  name="item_type_id"
                  label="Item Type"
                  defaultLabel="Select from options"
                  mandatory={true}
                  value={formik.values.item_type_id}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.item_type_id && formik.errors.item_type_id
                  }
                  disabled={disable}
                  placeholder="Select from options"
                >
                  {getAllItemTypeList?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_id}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div style={{ width: '30%' }}>
                <TextArea
                  name="description"
                  label="Description"
                  placeholder="Enter description"
                  value={formik.values.description}
                  onChange={formik.handleChange}
                  rows={5}
                  mandatory={true}
                  maxCharacterCount={100}
                  error={
                    formik.touched.description && formik.errors.description
                  }
                />
              </div>

            </div>

            <div className={Styles.itemField1}>
              <div style={{ width: '30%' }}>
                <Input
                  label="Rate"
                  placeholder="Enter rate"
                  name="rate"
                  mandatory={true}
                  value={formik.values.rate}
                  onChange={formik.handleChange}
                  error={formik.touched.rate && formik.errors.rate}
                />
              </div>
              <div style={{ width: '30%' }}>
                <AutoCompleteSelect
                  name="gst_id"
                  label="GST"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  value={formik.values.gst_id}
                  onChange={formik.handleChange}
                  error={formik.touched.gst_id && formik.errors.gst_id}
                  onSelect={(value) => {
                    formik.setFieldValue('gst_id', value);
                  }}
                  optionList={getAllGstList}
                  mandatory={true}
                />
              </div>
            </div>
            <div className={Styles.itemField1}>
              <div style={{ width: '30%' }}>
                <AutoCompleteSelect
                  name="uom_id"
                  label="UOM"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  value={formik.values.uom_id}
                  onChange={formik.handleChange}
                  error={formik.touched.uom_id && formik.errors.uom_id}
                  onSelect={(value) => {
                    formik.setFieldValue('uom_id', value);
                  }}
                  optionList={getAllUomList}
                  mandatory={true}
                  disabled={disable}
                />
              </div>
              <div style={{ width: '30%' }}>
                <AutoCompleteSelect
                  name="hsn_code_id"
                  label="HSN Code"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  value={formik.values.hsn_code_id}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.hsn_code_id && formik.errors.hsn_code_id
                  }
                  onSelect={(value) => {
                    formik.setFieldValue('hsn_code_id', value);
                  }}
                  optionList={getAllHsnList}
                  mandatory={true}
                  disabled={disable}
                />
              </div>
            </div>
            <div className={Styles.itemFields2}>
            <div style={{ width: '33%' }}>
              <AutoCompleteSelect
                name="brand_id"
                label="Brand"
                defaultLabel="Select from options"
                placeholder="Select from options"
                value={formik.values.brand_id}
                onChange={formik.handleChange}
                mandatory={true}
                error={formik.touched.brand_id && formik.errors.brand_id}
                onSelect={(value) => {
                  formik.setFieldValue('brand_id', value);
                }}
                optionList={getAllBrandList}
              />
            </div>
            </div>
          </div>
          <div className={Styles.buttonFields}>
            <div>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                type="submit"
              >
                Save
              </Button>
            </div>
          </div>
        </form>
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
export default ProductAdd;
