import React from 'react';
import Input from './Input';
import Styles from '../../styles/customAddress.module.scss';

const AddressComponenet = ({ formik }) => {
  return (
    <div>
      <div className={Styles.title}>
        <h3>Address</h3>
      </div>
      <div className={Styles.fieldContainer}>
        <div className={Styles.inputField}>
          <div style={{ width: '20%' }}>
            <Input
              label="Street"
              placeholder="Enter Street Name"
              name="address.street"
              value={formik.values.address.street}
              onChange={formik.handleChange}
              error={
                formik.touched.address?.street && formik.errors.address?.street
              }
            />
          </div>
          <div style={{ width: '20%' }}>
            <Input
              label="City"
              placeholder="Enter City Name"
              name="address.city"
              value={formik.values.address.city}
              onChange={formik.handleChange}
              error={
                formik.touched.address?.city && formik.errors.address?.city
              }
            />
          </div>
          <div style={{ width: '20%' }}>
            <Input
              label="State"
              placeholder="Enter State Name"
              name="address.state"
              value={formik.values.address.state}
              onChange={formik.handleChange}
              error={
                formik.touched.address?.state && formik.errors.address?.state
              }
            />
          </div>
        </div>
        <div className={Styles.inputField}>
          <div style={{ width: '20%' }}>
            <Input
              label="Country"
              placeholder="Enter Country Name"
              name="address.country"
              value={formik.values.address.country}
              onChange={formik.handleChange}
              error={
                formik.touched.address?.country &&
                formik.errors.address?.country
              }
            />
          </div>
          <div style={{ width: '20%' }}>
            <Input
              label="Pin Code"
              placeholder="Enter Pin Code"
              name="address.pin_code"
              value={formik.values.address.pin_code}
              onChange={formik.handleChange}
              error={
                formik.touched.address?.pin_code &&
                formik.errors.address?.pin_code
              }
            />
          </div>
        </div>
      </div>
    </div>
  );
};

export default AddressComponenet;
