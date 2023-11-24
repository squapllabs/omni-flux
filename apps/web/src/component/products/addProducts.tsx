import React, { useEffect, useState } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import hsnCode from '../../service/hsnCode-service';
import umoCode from '../../service/uom-service';
import brandCode from '../../service/brand-service';
import gstRateCode from '../../service/gst-service';
import itemTypes from '../../service/item-type';
import Styles from '../../styles/addItem.module.scss';
import Input from '../ui/Input';
import AddItem from '../../service/add-product';
import CustomSelect from '../ui/customSelect';
// import Button from '../menu/button';
interface FormValues {
  item_name: string;
  sub_sub_category_id: number;
  description: string;
  hsn_code_id: string;
  gst_id: string;
  uom_id: string;
  created_by: string | null;
  updated_by: string | null;
  item_type_item_id: string;
  brand_id: string;
}
interface HSNCode {
  code: string;
  hsn_code_id: number;
}

interface UOM {
  name: string;
  uom_id: number;
}

interface GSTRate {
  rate: string;
  gst_id: number;
}
interface ItemType {
  item_type_item_code: string;
  item_type_id: number;
}
interface Brand {
  brand_name: string;
  brand_id: number;
}

const AddProducts = () => {
  const formik = useFormik({
    initialValues: {
      item_name: '',
      sub_sub_category_id: 1,
      description: '',
      hsn_code_id: '',
      gst_id: '',
      uom_id: '',
      created_by: null,
      updated_by: null,
      item_type_item_id: '',
      brand_id: '',
    },
    onSubmit: async (values) => {
      try {
        await AddItem.addProduct(values);
      } catch (error) {
        console.log('Error in Sending data to item table', error);
      }
    },
  });

  const [hsnValues, setHSNValues] = useState<HSNCode[]>([]);
  const [uomValues, setUOMValues] = useState<UOM[]>([]);
  const [gstRate, setGstRate] = useState<GSTRate[]>([]);
  const [itemType, setItemType] = useState<ItemType[]>([]);
  const [brand, setBrand] = useState<Brand[]>([]);

  useEffect(() => {
    fetchData();
    fetchUOMData();
    fetchGstRate();
    fetchItemTypes();
    fetchBrandData();
  }, []);

  const fetchData = async () => {
    try {
      const data = await hsnCode.getAllHsnCode();
      const HSNOptions = data.data.map((option: HSNCode) => ({
        value: option.hsn_code_id,
        label: option.code,
      }));
      setHSNValues(HSNOptions);
    } catch (error) {
      console.log('Error in fetching HSN code data:', error);
    }
  };

  const fetchUOMData = async () => {
    try {
      const data = await umoCode.getAlluom();
      const UomOptions = data.data.map((option: UOM) => ({
        value: option.uom_id,
        label: option.name,
      }));
      setUOMValues(UomOptions);
    } catch (error) {
      console.log('Error in fetching HSN code data:', error);
    }
  };
  const fetchBrandData = async () => {
    try {
      const data = await brandCode.getAllBrand();
      const brandName = data.data.map((option: Brand) => ({
        value: option.brand_id,
        label: option.brand_name,
      }));
      setBrand(brandName);
    } catch (error) {
      console.log('Error in fetching Brand Data:', error);
    }
  };

  const fetchGstRate = async () => {
    try {
      const data = await gstRateCode.getAllGst();
      const GSTOptions = data.data.map((option: GSTRate) => ({
        value: option.gst_id,
        label: option.rate,
      }));
      setGstRate(GSTOptions);
    } catch (error) {
      console.log('Error in fetching HSN code data:', error);
    }
  };
  const fetchItemTypes = async () => {
    try {
      const data = await itemTypes.getAllItemsType();
      const ItemType = data.data.map((option: ItemType) => ({
        value: option.item_type_id,
        label: option.item_type_item_code,
      }));
      setItemType(ItemType);
      console.log(ItemType);
    } catch (error) {
      console.log('Error in fetching Item Type data:', error);
    }
  };

  const handleAddItem = () => {
    console.log('Item Added');
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.headingContainer}>
        <h2>Add Item</h2>
        <p>Add your raw materials ( Raw, Semi Finished & Finished).</p>
      </div>
      <div>
        <div>
          <form onSubmit={formik.handleSubmit}>
            <div className={Styles.formContainer}>
              <div className={Styles.itemContainer}>
                <div className={Styles.itemDetailHeading}>
                  <p>Item details</p>
                </div>
                <div className={Styles.inputContainer}>
                  <div>
                    <Input
                      label="Item Name"
                      name="item_name"
                      onChange={formik.handleChange}
                      value={formik.values.item_name}
                    />
                  </div>

                  <div>
                    <Input
                      label="Description"
                      name="description"
                      value={formik.values.description}
                      onChange={formik.handleChange}
                    />
                  </div>
                  <div>
                    <CustomSelect
                      label="Item Type"
                      options={itemType}
                      value={formik.values.item_type_item_id}
                      name="item_type_item_id"
                      onChange={formik.handleChange}
                      width="420px"
                    />
                  </div>
                </div>
              </div>
              <div className={Styles.separationLine}></div>
              <div className={Styles.itemContainer}>
                <div className={Styles.itemDetailHeading}>
                  <p>GST</p>
                </div>
                <div className={Styles.inputContainer}>
                  <CustomSelect
                    label="GST"
                    options={gstRate}
                    value={formik.values.gst_id}
                    name="gst_id"
                    onChange={formik.handleChange}
                    width="420px"
                  />
                </div>
              </div>
              <div className={Styles.separationLine}></div>
              <div className={Styles.itemContainer}>
                <div className={Styles.itemDetailHeading}>
                  <p>UOM</p>
                </div>
                <div className={Styles.inputContainer}>
                  <CustomSelect
                    label="UOM"
                    options={uomValues}
                    value={formik.values.uom_id}
                    onChange={formik.handleChange}
                    name="uom_id"
                    width="420px"
                  />
                </div>
              </div>
              <div className={Styles.separationLine}></div>
              <div className={Styles.itemContainer}>
                <div className={Styles.itemDetailHeading}>
                  <p>HSN Code</p>
                </div>
                <div className={Styles.inputContainer}>
                  <CustomSelect
                    label="HSN CODE"
                    options={hsnValues}
                    value={formik.values.hsn_code_id}
                    name="hsn_code_id"
                    onChange={formik.handleChange}
                    width="420px"
                  />
                </div>
              </div>
              <div className={Styles.separationLine}></div>
              <div className={Styles.itemContainer}>
                <div className={Styles.itemDetailHeading}>
                  <p>Brand</p>
                </div>
                <div className={Styles.inputContainer}>
                  <CustomSelect
                    label="Brand"
                    options={brand}
                    value={formik.values.brand_id}
                    name="brand_id"
                    onChange={formik.handleChange}
                    width="420px"
                  />
                </div>
              </div>
            </div>
            {/* <Button
              type="submit"
              text="Add item"
              backgroundColor="#6941C6"
              onClick={handleAddItem}
            /> */}
          </form>
        </div>
      </div>
    </div>
  );
};

export default AddProducts;
