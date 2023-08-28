import React, { useState } from 'react';
import Styles from '../../styles/siteForm.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import { useNavigate } from 'react-router';
import CustomSnackBar from '../ui/customSnackBar';
import TextArea from '../ui/CustomTextArea';
import { getCreateValidateyup } from '../../helper/constants/contractor-constants';
import { createSite } from '../../hooks/site-hooks';

const ContractorForm = () => {
  const { mutate: createNewSite } = createSite();
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const navigate = useNavigate();
  const [initialValues, setInitialValues] = useState({
    name: '',
    code: '',
    mobile_number: '',
    description: '',
    contact_number: '',
    address: {
      street: '',
      city: '',
      state: '',
      pin_code: '',
      country: '',
    },
  });

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const validationSchema = getCreateValidateyup(Yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    onSubmit: (values) => {
      const Object: any = {
        name: values.name,
        code: values.code,
        mobile_number: values.mobile_number,
        description: values.description,
        contact_number: values.contact_number,
        type: 'Contractor',
        address: {
          street: values.address.street,
          city: values.address.city,
          state: values.address.state,
          pin_code: values.address.pin_code,
          country: values.address.country,
        },
      };
      createNewSite(Object, {
        onSuccess: (data: any) => {
          if (data?.status === true) {
            setMessage('Contractor created');
            setOpenSnack(true);
            setInterval(() => {
              navigate('/site');
            }, 1000);
          }
        },
      });
    },
  });

  return (
    <div className={Styles.container}>
      <div className={Styles.textContent}>
        <h3>Add Contractor</h3>
        <span className={Styles.content}>Add your contractor</span>
      </div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.inputFieldMain}>
          <div className={Styles.inputFields}>
            <div className={Styles.input}>
              <Input
                label="Contractor Name"
                placeholder="Enter contractor name"
                name="name"
                mandatory={true}
                value={formik.values.name}
                onChange={formik.handleChange}
                error={formik.touched.name && formik.errors.name}
              />
            </div>
            <div className={Styles.input}>
              <Input
                label="Code"
                placeholder="Enter code"
                name="code"
                mandatory={true}
                value={formik.values.code}
                onChange={formik.handleChange}
                error={formik.touched.code && formik.errors.code}
              />
            </div>
          </div>
          <div className={Styles.inputFields}>
            <div className={Styles.input}>
              <Input
                label="Mobile Number"
                placeholder="Enter mobile number"
                name="mobile_number"
                value={formik.values.mobile_number}
                onChange={formik.handleChange}
                error={
                  formik.touched.mobile_number && formik.errors.mobile_number
                }
              />
            </div>
            <div className={Styles.input}>
              <Input
                label="Contact Number"
                placeholder="Enter contact number"
                name="contact_number"
                value={formik.values.contact_number}
                onChange={formik.handleChange}
                error={
                  formik.touched.contact_number && formik.errors.contact_number
                }
              />
            </div>
          </div>
        </div>
        <div className={Styles.inputFieldMainFour}>
          <div className={Styles.inputFieldsFour}>
            <div className={Styles.input}>
              <TextArea
                name="description"
                label="Description"
                placeholder="Enter description"
                value={formik.values.description}
                onChange={formik.handleChange}
                rows={4}
                maxCharacterCount={100}
                mandatory={true}
              />
            </div>
          </div>
        </div>
        <div className={Styles.inputFieldMainOne}>
          <h4>Address</h4>
        </div>
        <div className={Styles.inputFieldMainTwo}>
          <div className={Styles.inputFieldsTwo}>
            <div className={Styles.input}>
              <Input
                label="Address line"
                placeholder="Enter address 1"
                name="address.street"
                value={formik.values.address.street}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.street &&
                  formik.errors.address?.street
                }
              />
            </div>
            <div className={Styles.input}>
              <Input
                label="City"
                placeholder="Enter address"
                name="address.city"
                value={formik.values.address.city}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.city && formik.errors.address?.city
                }
              />
            </div>
          </div>
          <div className={Styles.inputFieldsTwo}>
            <div className={Styles.input}>
              <Input
                label="State"
                placeholder="Enter city"
                name="address.state"
                value={formik.values.address.state}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.state && formik.errors.address?.state
                }
              />
            </div>
            <div className={Styles.input}>
              <Input
                label="Pincode "
                placeholder="Enter pincode"
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
        <div className={Styles.inputFieldMainThree}>
          <div className={Styles.inputFieldsThree}>
            <div className={Styles.input}>
              <Input
                label="Country"
                placeholder="Enter country name"
                name="address.country"
                value={formik.values.address.country}
                onChange={formik.handleChange}
                error={
                  formik.touched.address?.country &&
                  formik.errors.address?.country
                }
              />
            </div>
            <div></div>
          </div>
        </div>
        <div className={Styles.submitButton}>
          <Button
            className={Styles.resetButton}
            type="submit"
            shape="rectangle"
            justify="center"
            onClick={() => navigate('/contractor')}
          >
            Back
          </Button>
          <Button
            type="submit"
            color="primary"
            shape="rectangle"
            justify="center"
          >
            Submit
          </Button>
        </div>
      </form>
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

export default ContractorForm;
