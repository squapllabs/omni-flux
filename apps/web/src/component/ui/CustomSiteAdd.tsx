import React, { useState } from 'react';
import Styles from '../../styles/projectForm.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import TextArea from '../ui/CustomTextArea';
import { getCreateValidateyup as siteValidate } from '../../helper/constants/site-constants';
import { useInstantCreateSite } from '../../hooks/site-hooks';
import CustomSnackbar from '../ui/customSnackBar';

const CustomSiteAdd = (props: { setOpen: any; open: any }) => {
  const { setOpen } = props;
  const { mutate: createNewSite } = useInstantCreateSite();
  const validationSchemaSite = siteValidate(Yup);
  const [message, setMessage] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [siteinitialValues, setsiteInitialValues] = useState({
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

  const formikTwo = useFormik({
    initialValues: siteinitialValues,
    validationSchema: validationSchemaSite,
    enableReinitialize: true,
    onSubmit: (values) => {
      const Object: any = {
        name: values.name,
        code: values.code,
        mobile_number: values.mobile_number,
        description: values.description,
        contact_number: values.contact_number,
        type: 'Site',
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
            setMessage('Site created');
            setOpenSnack(true);
            setTimeout(() => {
              handleCloseSiteForm();
            }, 1000);
          }
        },
      });
    },
  });

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const handleCloseSiteForm = () => {
    setOpen(false);
  };

  return (
    <div>
      <div>
        <div>
          <form onSubmit={formikTwo.handleSubmit}>
            <div className={Styles.overflow}>
              <div className={Styles.site}>
                <div className={Styles.siteRow}>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Site Name"
                      placeholder="Enter site name"
                      name="name"
                      value={formikTwo.values.name}
                      onChange={formikTwo.handleChange}
                      error={formikTwo.touched.name && formikTwo.errors.name}
                      mandatory={true}
                    />
                  </div>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Code"
                      placeholder="Enter code"
                      name="code"
                      value={formikTwo.values.code}
                      onChange={formikTwo.handleChange}
                      error={formikTwo.touched.code && formikTwo.errors.code}
                      mandatory={true}
                    />
                  </div>
                </div>
                <div className={Styles.siteRow}>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Mobile Number"
                      placeholder="Enter mobile number"
                      name="mobile_number"
                      value={formikTwo.values.mobile_number}
                      onChange={formikTwo.handleChange}
                      error={
                        formikTwo.touched.mobile_number &&
                        formikTwo.errors.mobile_number
                      }
                    />
                  </div>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Contact Number"
                      placeholder="Enter contact number"
                      name="contact_number"
                      value={formikTwo.values.contact_number}
                      onChange={formikTwo.handleChange}
                      error={
                        formikTwo.touched.contact_number &&
                        formikTwo.errors.contact_number
                      }
                    />
                  </div>
                </div>
              </div>
              <div className={Styles.siteDes}>
                <div>
                  <div style={{ width: '41%' }}>
                    <TextArea
                      name="description"
                      label="Description"
                      placeholder="Enter description"
                      value={formikTwo.values.description}
                      onChange={formikTwo.handleChange}
                      rows={5}
                      maxCharacterCount={100}
                    />
                  </div>
                </div>
              </div>
              <div className={Styles.siteDes}>
                <h4>Address</h4>
              </div>
              <div className={Styles.site}>
                <div className={Styles.siteRow}>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Address line"
                      placeholder="Enter address 1"
                      name="address.street"
                      value={formikTwo.values.address.street}
                      onChange={formikTwo.handleChange}
                      error={
                        formikTwo.touched.address?.street &&
                        formikTwo.errors.address?.street
                      }
                    />
                  </div>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="City"
                      placeholder="Enter address"
                      name="address.city"
                      value={formikTwo.values.address.city}
                      onChange={formikTwo.handleChange}
                      error={
                        formikTwo.touched.address?.city &&
                        formikTwo.errors.address?.city
                      }
                    />
                  </div>
                </div>
                <div className={Styles.siteRow}>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="State"
                      placeholder="Enter city"
                      name="address.state"
                      value={formikTwo.values.address.state}
                      onChange={formikTwo.handleChange}
                      error={
                        formikTwo.touched.address?.state &&
                        formikTwo.errors.address?.state
                      }
                    />
                  </div>
                  <div style={{ width: '40%' }}>
                    <Input
                      label="Pincode "
                      placeholder="Enter pincode"
                      name="address.pin_code"
                      value={formikTwo.values.address.pin_code}
                      onChange={formikTwo.handleChange}
                      error={
                        formikTwo.touched.address?.pin_code &&
                        formikTwo.errors.address?.pin_code
                      }
                    />
                  </div>
                </div>
              </div>
              <div className={Styles.siteDes}>
                <div>
                  <div style={{ width: '41%' }}>
                    <Input
                      label="Country"
                      placeholder="Enter country name"
                      name="address.country"
                      value={formikTwo.values.address.country}
                      onChange={formikTwo.handleChange}
                      error={
                        formikTwo.touched.address?.country &&
                        formikTwo.errors.address?.country
                      }
                    />
                  </div>
                  <div></div>
                </div>
              </div>
            </div>
            <div className={Styles.dividerStyle}></div>
            <div className={Styles.formButton}>
              <div>
                <Button
                  className={Styles.cancelButton}
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={handleCloseSiteForm}
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
      <CustomSnackbar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default CustomSiteAdd;
