import React, { useState } from 'react';
import Styles from '../../styles/addItem.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Button from '../ui/Button';
// import CustomSnackBar from '../ui/customSnackBar';
import TextArea from '../ui/CustomTextArea';
import Input from '../ui/Input';
import Select from '../ui/selectNew';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import { useGetAllBrandForDrop } from '../../hooks/item-type-hooks';
import { useGetAllGstForDrop } from '../../hooks/gst-hooks';
import { useGetAllUomDrop } from '../../hooks/uom-hooks';
import { useGetAllHsnForDrop } from '../../hooks/hsnCode-hooks';
import { useInstantCreateItem } from '../../hooks/item-hooks';
import { useGetBymasertDataType } from '../../hooks/masertData-hook';
import { getCreateValidateyup } from '../../helper/constants/item-constants';

const InstantItemAdd = (props: {
  isVissible: any;
  onAction: any;
  setMessage: any;
  setOpenSnack: any;
}) => {
  const { onAction, setMessage, setOpenSnack } = props;
  const { data: getAllItemTypeList = [] } = useGetBymasertDataType('IMTY');
  const { data: getAllGstList = [] } = useGetAllGstForDrop();
  const { data: getAllUomList = [] } = useGetAllUomDrop();
  const { data: getAllHsnList = [] } = useGetAllHsnForDrop();
  const { data: getAllBrandList = [] } = useGetAllBrandForDrop();
  const { mutate: createNewItem } = useInstantCreateItem();
  const validationSchema = getCreateValidateyup(Yup);
  const [initialValues, setInitialValues] = useState({
    item_id: '',
    item_name: '',
    description: '',
    hsn_code_id: '',
    gst_id: '',
    uom_id: '',
    item_type_id: '',
    brand_id: '',
    rate: '',
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      const Object: any = {
        item_name: values.item_name,
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
            handleCloseForm();
          }
        },
      });
    },
  });

  const handleCloseForm = () => {
    onAction(false);
    formik.resetForm();
  };
  return (
    <div className={Styles.container}>
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
                  error={formik.touched.item_name && formik.errors.item_name}
                />
              </div>
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
            </div>
            <div className={Styles.itemField1}>
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
            </div>

            <div className={Styles.itemField1}>
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
                />
              </div>
            </div>
            <div className={Styles.itemField1}>
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
                />
              </div>
              <div style={{ width: '30%' }}>
                <AutoCompleteSelect
                  name="brand_id"
                  label="Brand"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  value={formik.values.brand_id}
                  onChange={formik.handleChange}
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
                className={Styles.cancelButton}
                shape="rectangle"
                justify="center"
                size="small"
                onClick={handleCloseForm}
              >
                Cancel
              </Button>
            </div>
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
    </div>
  );
};

export default InstantItemAdd;
