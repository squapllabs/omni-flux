import React, { useState, useEffect } from 'react';
import Styles from '../../styles/siteForm.module.scss';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import { useNavigate } from 'react-router';
import CustomSnackBar from '../ui/customSnackBar';
import TextArea from '../ui/CustomTextArea';
import { editCreateValidateyup } from '../../helper/constants/site-constants';
import { useUpdateSite, useGetBySiteId } from '../../hooks/site-hooks';
import { useParams } from 'react-router-dom';
import ProjectSubheader from '../project/projectSubheader';

const ContractorForm = () => {
  const routeParams = useParams();
  const { data: getOneSiteData, isLoading } = useGetBySiteId(
    Number(routeParams?.id)
  );
  const { mutate: updateNewSite } = useUpdateSite();
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
  useEffect(() => {
    if (getOneSiteData) {
      setInitialValues({
        name: getOneSiteData?.name || '',
        code: getOneSiteData?.code || '',
        mobile_number: getOneSiteData?.mobile_number || '',
        description: getOneSiteData?.description || '',
        contact_number: getOneSiteData?.contact_number || '',
        address: {
          street: getOneSiteData?.address?.street || '',
          city: getOneSiteData?.address?.city || '',
          state: getOneSiteData?.address?.state || '',
          country: getOneSiteData?.address?.country || '',
          pin_code: getOneSiteData?.address?.pin_code || '',
        },
      });
    }
  }, [getOneSiteData]);

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const validationSchema = editCreateValidateyup(Yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
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
        site_contractor_id: Number(routeParams?.id),
      };
      updateNewSite(Object, {
        onSuccess: (data: any) => {
          if (data?.status === true) {
            setMessage('Contractor edited');
            setOpenSnack(true);
            setTimeout(() => {
              navigate('/settings');
            }, 1000);
          }
        },
      });
    },
  });
  if (isLoading) {
    return <div>Loading...</div>;
  }

  return (
    <div className={Styles.container}>
      <ProjectSubheader
        navigation={'/settings'}
        description=""
        title="Edit Contractor"
      />
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
                disabled={true}
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
                mandatory={true}
                maxCharacterCount={100}
                error={formik.touched.description && formik.errors.description}
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
