import React, { useEffect, useState } from 'react';
import { Formik, Form, Field, ErrorMessage } from 'formik';
import * as Yup from 'yup';
import hsnCode from '../../service/hsnCode-service';
import umoCode from '../../service/uom-service';
import gstRateCode from '../../service/gst-service';
import Styles from '../../styles/addItem.module.scss';
import Input from '../ui/Input';

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
    itemName: '',
    itemDescription: '',
    uom: '',
    gst: '',
    hsn_code: '',
  };

  const validationSchema = Yup.object().shape({
    itemCode: Yup.string().required('Item code is required'),
    itemName: Yup.string().required('Item name is required'),

    itemDescription: Yup.string()
      .max(200, 'Item description must not exceed 200 characters')
      .required('Item description is required'),
    uom: Yup.string().required('Item name is required'),
    gst: Yup.string().required('Item name is required'),
    hsn_code: Yup.string().required('Item name is required'),
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
  };

  return (
    <div className={Styles.container}>
      <div className={Styles.headingContainer}>
        <h2>Add Item</h2>
        <p>Add your raw materials ( Raw, Semi Finished & Finished).</p>
      </div>

      <div>
        <div>
          <Formik
            initialValues={initialValues}
            onSubmit={onSubmit}
            validationSchema={validationSchema}
            enableReinitialize
          >
            {({ values }) => (
              <Form>
                <div className={Styles.formContainer}>
                  <div className={Styles.itemContainer}>
                    <div className={Styles.itemDetailHeading}>
                      <p>Item details</p>
                    </div>
                    <div className={Styles.inputContainer}>
                      <div>
                        <Input label="Item Code" />
                      </div>

                      <div>
                        <Input label="Name of Item" />
                      </div>

                      <div>
                        <label htmlFor="itemDescription">
                          Item Description
                        </label>
                        <p className={Styles.descriptionCaption}>
                          Give short details of the Item
                        </p>
                        <Field
                          as="textarea"
                          id="itemDescription"
                          name="itemDescription"
                          onKeyUp={(
                            e: React.ChangeEvent<HTMLTextAreaElement>
                          ) =>
                            console.log(
                              `Characters left: ${200 - e.target.value.length}`
                            )
                          }
                        />
                        <div className={Styles.charCount}>
                          {` ${
                            200 - values.itemDescription.length
                          } Characters left`}
                        </div>
                        <ErrorMessage
                          name="itemDescription"
                          component="div"
                          className={Styles.error}
                        />
                      </div>
                    </div>
                  </div>

                  <div className={Styles.itemContainer}>
                    <div className={Styles.itemDetailHeading}>
                      <p>Unit of Measurment(Uom)</p>
                    </div>
                    <div className={Styles.inputContainer}>
                      <div className={Styles['select-container']}>
                        <label htmlFor="selectedOption">UOM</label>
                        <Field
                          as="select"
                          id="selectedOption"
                          name="selectedOption"
                        >
                          <option value="">Select an option</option>
                          {uomValues.map((option) => (
                            <option key={option.name} value={option.name}>
                              {option.name}
                            </option>
                          ))}
                        </Field>
                      </div>
                    </div>
                  </div>
                  <div className={Styles.separationLine}></div>
                  <div className={Styles.itemContainer}>
                    <div className={Styles.itemDetailHeading}>
                      <p>GST</p>
                    </div>
                    <div className={Styles.inputContainer}>
                      <div className={Styles['select-container']}>
                        <label htmlFor="selectedOption">GST</label>
                        <Field as="select" id="gst" name="gst">
                          <option value="">Select an option</option>
                          {gstRate.map((option) => (
                            <option key={option.rate} value={option.rate}>
                              {option.rate}
                            </option>
                          ))}
                        </Field>
                        <ErrorMessage
                          name="gst"
                          component="div"
                          className={Styles.error}
                        />
                      </div>
                    </div>
                  </div>
                  <div className={Styles.separationLine}></div>
                  <div className={Styles.itemContainer}>
                    <div className={Styles.itemDetailHeading}>
                      <p>HSN Code</p>
                    </div>
                    <div className={Styles.inputContainer}>
                      <div className={Styles['select-container']}>
                        <label htmlFor="selectedOption">HSN Code</label>
                        <Field as="select" id="hsn_code" name="hsn_code">
                          <option value="">Select an option</option>
                          {hsnValues.map((option) => (
                            <option key={option.code} value={option.code}>
                              {option.code}
                            </option>
                          ))}
                        </Field>
                        <ErrorMessage
                          name="hsn_code"
                          component="div"
                          className={Styles.error}
                        />
                      </div>
                    </div>
                  </div>
                </div>

                {/* <button type="submit">Submit</button> */}
              </Form>
            )}
          </Formik>
        </div>
      </div>
    </div>
  );
};

export default AddProducts;
