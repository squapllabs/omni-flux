import React, { useEffect, useState } from 'react';
import { Formik, Form, Field, ErrorMessage } from 'formik';
import * as Yup from 'yup';
import './addProduct.css'; // Import CSS file for styling
import hsnCode from '../../service/hsnCode-service';
import umoCode from '../../service/uom-service';
import gstRateCode from '../../service/gst-service';

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
    <div className="form-container">
      <h1>Add Product</h1>
      <Formik
        initialValues={initialValues}
        onSubmit={onSubmit}
        validationSchema={validationSchema}
      >
        <Form>
          <div className="form-group">
            <label htmlFor="itemCode">Item Code</label>
            <Field
              type="text"
              id="itemCode"
              name="itemCode"
              className="form-input"
            />
            <ErrorMessage name="itemCode" component="div" className="error" />
          </div>

          <div className="form-group">
            <label htmlFor="description">Product Description</label>
            <Field
              type="text"
              id="description"
              name="description"
              className="form-input"
            />
            <ErrorMessage
              name="description"
              component="div"
              className="error"
            />
          </div>

          <div className="form-group">
            <label htmlFor="brand">Product Brand</label>
            <Field type="text" id="brand" name="brand" className="form-input" />
            <ErrorMessage name="brand" component="div" className="error" />
          </div>

          <div className="form-group">
            <label htmlFor="uom">UOM</label>
            <Field as="select" id="uom" name="uom" className="form-input">
              <option value="">Select UOM</option>
              {uomValues.map((umo) => (
                <option key={umo.name} value={umo.name}>
                  {umo.name}
                </option>
              ))}
            </Field>
            <ErrorMessage name="uom" component="div" className="error" />
          </div>

          <div className="form-group">
            <label htmlFor="packSize">Pack Size</label>
            <Field
              type="text"
              id="packSize"
              name="packSize"
              className="form-input"
            />
            <ErrorMessage name="packSize" component="div" className="error" />
          </div>

          <div className="form-group">
            <label htmlFor="hsnCode">HSN Code</label>
            <Field
              as="select"
              id="hsnCode"
              name="hsnCode"
              className="form-input"
            >
              {hsnValues.map((hsn) => (
                <option key={hsn.code} value={hsn.code}>
                  {hsn.code}
                </option>
              ))}
            </Field>
            <ErrorMessage name="hsnCode" component="div" className="error" />
          </div>

          <div className="form-group">
            <label htmlFor="taxRate">Tax Rate</label>
            <Field
              as="select"
              id="taxRate"
              name="taxRate"
              className="form-input"
            >
              <option value="">Select GST</option>
              {gstRate.map((gst) => (
                <option key={gst.rate} value={gst.rate}>
                  {gst.rate}
                </option>
              ))}
            </Field>
            <ErrorMessage name="taxRate" component="div" className="error" />
          </div>

          <div className="form-group">
            <label htmlFor="mrp">MRP</label>
            <Field type="number" id="mrp" name="mrp" className="form-input" />
            <ErrorMessage name="mrp" component="div" className="error" />
          </div>

          <div className="form-group">
            <label htmlFor="currentStock">Current Stock</label>
            <Field
              type="number"
              id="currentStock"
              name="currentStock"
              className="form-input"
            />
            <ErrorMessage
              name="currentStock"
              component="div"
              className="error"
            />
          </div>

          <div className="form-group">
            <label htmlFor="minimumStock">Minimum Stock</label>
            <Field
              type="number"
              id="minimumStock"
              name="minimumStock"
              className="form-input"
            />
            <ErrorMessage
              name="minimumStock"
              component="div"
              className="error"
            />
          </div>

          <button type="submit">Add Product</button>
        </Form>
      </Formik>
    </div>
  );
};

export default AddProducts;
