import React, { useEffect, useState } from 'react';
import { Formik, Form, Field, ErrorMessage } from 'formik';
import * as Yup from 'yup';
import hsnCode from '../../service/hsnCode-service';
import umoCode from '../../service/uom-service';
import gstRateCode from '../../service/gst-service';
import Styles from '../../styles/addItem.module.scss';

interface HSNCode {
  code: string;
}

interface UOM {
  name: string;
}

interface GSTRate {
  rate: string;
}

const AddProducts = () => {
  const initialValues = {
    itemCode: '',
    description: '',
    brand: '',
    uom: '',
    packSize: '',
    hsnCode: '',
    taxRate: '',
    mrp: '',
    currentStock: '',
    minimumStock: '',
  };

  const validationSchema = Yup.object({
    itemCode: Yup.string().required('Item Code is required'),
    description: Yup.string().required('Product Description is required'),
    brand: Yup.string().required('Product Brand is required'),
    uom: Yup.string().required('UOM is required'),
    packSize: Yup.string().required('Pack Size is required'),
    hsnCode: Yup.number().required('HSN Code is required'),
    taxRate: Yup.number().required('Tax Rate is required'),
    mrp: Yup.number().required('MRP is required'),
    currentStock: Yup.number().required('Current Stock is required'),
    minimumStock: Yup.number().required('Minimum Stock is required'),
  });

  const [hsnValues, setHSNValues] = useState<HSNCode[]>([]);
  const [uomValues, setUOMValues] = useState<UOM[]>([]);
  const [gstRate, setGstRate] = useState<GSTRate[]>([]);

  useEffect(() => {
    fetchData();
    fetchUOMData();
    fetchGstRate();
  }, []);

  const fetchData = async () => {
    try {
      const data = await hsnCode.getAllHsnCode();
      setHSNValues(data.data);
    } catch (error) {
      console.log('Error in fetching HSN code data:', error);
    }
  };

  const fetchUOMData = async () => {
    try {
      const data = await umoCode.getAlluom();
      setUOMValues(data.data);
    } catch (error) {
      console.log('Error in fetching UOM data:', error);
    }
  };

  const fetchGstRate = async () => {
    try {
      const data = await gstRateCode.getAllGst();
      setGstRate(data.data);
    } catch (error) {
      console.log('Error in fetching GST rate data:', error);
    }
  };

  const onSubmit = (values: typeof initialValues) => {
    console.log(values);
    // Handle form submission logic here
  };

  return (
    <div className={Styles.co}>
      <div className={Styles.headingContainer}>
        <h2>Add Item</h2>
        <p>Add your raw materials ( Raw, Semi Finished & Finished).</p>
      </div>
    </div>
  );
};

export default AddProducts;
