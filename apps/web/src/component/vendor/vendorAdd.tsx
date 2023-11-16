import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import TextArea from '../ui/CustomTextArea';
import Styles from '../../styles/vendor.module.scss';
import { useNavigate, useParams, useLocation } from 'react-router-dom';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import CustomSnackBar from '../ui/customSnackBar';
import { getBymasertDataType } from '../../hooks/masertData-hook';
import Select from '../ui/selectNew';
import {
  getVendorCreationYupschema,
  getVendorEditYupschema,
} from '../../helper/constants/vendor-constants';
import { useCreateVendor, useUpdateVendor } from '../../hooks/vendor-hooks';
import vendorService from '../../service/vendor-service';
import ProjectSubheader from '../project/projectSubheader';

const AddVendor = () => {
  const navigate = useNavigate();
  const routeParams = useParams();
  const location = useLocation();
  const currentPath = location.state.path;
  const locationState = location.state || {};
  const projectId = locationState.project_id || null;
  const indentId = locationState.indent_id || null;
  const { mutate: createNewVendor } = useCreateVendor();
  const { mutate: useUpdateVendors } = useUpdateVendor();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userData: any = encryptedData.userData;
  const { data: getAllPaymentTypeList = [] } = getBymasertDataType('PPM');
  const { data: getAllVendorTypeList = [] } = getBymasertDataType('VNC');
  const { data: getAllCurrencyTypeList = [] } = getBymasertDataType('CRTYP');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const validationSchema = routeParams?.id
    ? getVendorEditYupschema(Yup)
    : getVendorCreationYupschema(Yup);
  const [initialValues, setInitialValues] = useState({
    vendor_id: '',
    vendor_name: '',
    code: '',
    contact_person: '',
    contact_email: '',
    contact_phone_no: '',
    address: {
      street: '',
      city: '',
      state: '',
      country: '',
      pin_code: '',
    },
    bank_account_details: {
      account_no: '',
      ifsc_code: '',
      acc_holder_name: '',
      bank_name: '',
    },
    preferred_payment_method_id: '',
    currency: '',
    payment_terms: '',
    vendor_category_id: '',
    lead_time: '',
    minimum_order_quantity: '',
    notes: '',
    created_by: '',
    tax_id: '',
  });

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  useEffect(() => {
    if (Number(routeParams?.id)) {
      const fetchOne = async () => {
        const Vendordata = await vendorService.getOneVendorById(
          Number(routeParams?.id)
        );
        setInitialValues({
          vendor_id: Vendordata?.data?.vendor_id,
          vendor_name: Vendordata?.data?.vendor_name,
          code: Vendordata?.data?.code,
          contact_person: Vendordata?.data?.contact_person,
          contact_email: Vendordata?.data?.contact_email,
          contact_phone_no: Vendordata?.data?.contact_phone_no,
          address: {
            street: Vendordata?.data?.address?.street || '',
            city: Vendordata?.data?.address?.city || '',
            state: Vendordata?.data?.address?.state || '',
            country: Vendordata?.data?.address?.country || '',
            pin_code: Vendordata?.data?.address?.pin_code || '',
          },
          bank_account_details: {
            account_no: Vendordata?.data?.bank_account_details.account_no,
            ifsc_code: Vendordata?.data?.bank_account_details.ifsc_code,
            acc_holder_name:
              Vendordata?.data?.bank_account_details.acc_holder_name,
            bank_name: Vendordata?.data?.bank_account_details.bank_name,
          },
          preferred_payment_method_id:
            Vendordata?.data?.preferred_payment_method_id,
          currency: Vendordata?.data?.currency,
          payment_terms: Vendordata?.data?.payment_terms,
          vendor_category_id: Vendordata?.data?.vendor_category_id,
          lead_time: Vendordata?.data?.lead_time,
          minimum_order_quantity: Vendordata?.data?.minimum_order_quantity,
          notes: Vendordata?.data?.notes,
          created_by: userData.user_id,
          tax_id: Vendordata?.data?.tax_id,
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
      const Object: any = {
        vendor_name: values.vendor_name,
        code: values.code,
        contact_person: values.contact_person,
        contact_email: values.contact_email,
        contact_phone_no: values.contact_phone_no,
        address: {
          street: values.address.street,
          city: values.address.city,
          state: values.address.state,
          country: values.address.country,
          pin_code: values.address.pin_code,
        },
        bank_account_details: {
          account_no: values.bank_account_details.account_no,
          ifsc_code: values.bank_account_details.ifsc_code,
          acc_holder_name: values.bank_account_details.acc_holder_name,
          bank_name: values.bank_account_details.bank_name,
        },
        preferred_payment_method_id: Number(values.preferred_payment_method_id),
        currency: values.currency,
        payment_terms: values.payment_terms,
        vendor_category_id: Number(values.vendor_category_id),
        lead_time: values.lead_time,
        minimum_order_quantity: Number(values.minimum_order_quantity),
        notes: values.notes,
        created_by: userData.user_id,
        tax_id: values.tax_id,
        vendor_id: Number(routeParams?.id) ? Number(routeParams?.id) : '',
      };
      if (Number(routeParams?.id)) {
        useUpdateVendors(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setMessage('Vendor edited');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      } else {
        createNewVendor(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setMessage('Vendor created');
              setOpenSnack(true);
              if (projectId !== null) {
                navigate('/purchase-request-add', {
                  state: { project_id: projectId, indent_id: indentId },
                });
              } else {
                setTimeout(() => {
                  navigate('/settings');
                }, 1000);
              }
            }
          },
        });
      }
    },
  });

  return (
    <div className={Styles.container}>
      <div>
        <ProjectSubheader
          title={routeParams.id ? 'VENDOR EDIT' : 'NEW VENDOR'}
          navigation={
            currentPath === '/vendor-list' ? '/vendor-list' : '/settings'
          }
        />
      </div>
      <div className={Styles.box}></div>
      {/* <div className={Styles.dividerStyle}></div> */}
      <div className={Styles.form}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.box1}>
            <h4>Vendor Details</h4>
          </div>
          <div className={Styles.inputFieldMain}>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="vendor_name"
                  label="Vendor Name"
                  placeholder="Enter Vendor Name"
                  mandatory={true}
                  value={formik.values.vendor_name}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.vendor_name && formik.errors.vendor_name
                  }
                />
              </div>
              <div>
                <Input
                  name="code"
                  label="Vendor Code"
                  placeholder="Enter Vendor Code"
                  mandatory={true}
                  value={formik.values.code}
                  onChange={formik.handleChange}
                  width="250px"
                  error={formik.touched.code && formik.errors.code}
                />
              </div>
              <div>
                <Input
                  name="contact_person"
                  label="Contact Person Name"
                  placeholder="Enter Contact Person Name"
                  mandatory={true}
                  value={formik.values.contact_person}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.contact_person &&
                    formik.errors.contact_person
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="contact_email"
                  label="Contact Email"
                  placeholder="Enter Contact Eamil"
                  mandatory={true}
                  value={formik.values.contact_email}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.contact_email && formik.errors.contact_email
                  }
                />
              </div>
              <div>
                <Input
                  name="contact_phone_no"
                  label="Contact Phone Number"
                  placeholder="Enter Contact Phone Number"
                  mandatory={true}
                  value={formik.values.contact_phone_no}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.contact_phone_no &&
                    formik.errors.contact_phone_no
                  }
                />
              </div>
              <div>
                <Input
                  name="address.street"
                  label="Street"
                  placeholder="Enter Street"
                  value={formik.values.address.street}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.address?.street &&
                    formik.errors.address?.street
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="address.city"
                  label="City"
                  placeholder="Enter City"
                  value={formik.values.address.city}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.address?.city && formik.errors.address?.city
                  }
                />
              </div>

              <div>
                <Input
                  name="address.state"
                  label="State"
                  placeholder="Enter State"
                  value={formik.values.address.state}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.address?.state &&
                    formik.errors.address?.state
                  }
                />
              </div>
              <div>
                <Input
                  name="address.country"
                  label="Country"
                  placeholder="Enter Country"
                  value={formik.values.address.country}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.address?.country &&
                    formik.errors.address?.country
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRowPin}>
              <Input
                name="address.pin_code"
                label="Pincode"
                placeholder="Enter Pincode"
                value={formik.values.address.pin_code}
                onChange={formik.handleChange}
                width="250px"
                error={
                  formik.touched.address?.pin_code &&
                  formik.errors.address?.pin_code
                }
              />
            </div>

            <div className={Styles.box1}>
              <h4>Bank and Payment Details</h4>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="bank_account_details.account_no"
                  label="Account Number"
                  mandatory={true}
                  placeholder="Enter Account Number"
                  value={formik.values.bank_account_details.account_no}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.bank_account_details?.account_no &&
                    formik.errors.bank_account_details?.account_no
                  }
                />
              </div>
              <div>
                <Input
                  name="bank_account_details.ifsc_code"
                  label="IFSC Code"
                  mandatory={true}
                  placeholder="Enter Ifsc Code"
                  value={formik.values.bank_account_details.ifsc_code}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.bank_account_details?.ifsc_code &&
                    formik.errors.bank_account_details?.ifsc_code
                  }
                />
              </div>
              <div>
                <Input
                  name="bank_account_details.acc_holder_name"
                  label="Account Holder Name"
                  mandatory={true}
                  placeholder="Enter Account Holder Name"
                  value={formik.values.bank_account_details.acc_holder_name}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.bank_account_details?.acc_holder_name &&
                    formik.errors.bank_account_details?.acc_holder_name
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="bank_account_details.bank_name"
                  label="Bank Name"
                  placeholder="Enter Bank Name"
                  value={formik.values.bank_account_details.bank_name}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.bank_account_details?.bank_name &&
                    formik.errors.bank_account_details?.bank_name
                  }
                />
              </div>
              <div>
                <Select
                  name="preferred_payment_method_id"
                  label="Preffered Payment Type"
                  defaultLabel="Select from options"
                  width="250px"
                  mandatory={true}
                  value={formik.values.preferred_payment_method_id}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.preferred_payment_method_id &&
                    formik.errors.preferred_payment_method_id
                  }
                >
                  {getAllPaymentTypeList?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_id}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div>
                <Select
                  name="currency"
                  label="Currency"
                  defaultLabel="Select from options"
                  mandatory={true}
                  width="250px"
                  value={formik.values.currency}
                  onChange={formik.handleChange}
                  error={formik.touched.currency && formik.errors.currency}
                >
                  {getAllCurrencyTypeList?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_name}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fieldRowLast}>
              <TextArea
                name="payment_terms"
                label="Payment Terms"
                placeholder="Enter description"
                width="35%"
                value={formik.values.payment_terms}
                onChange={formik.handleChange}
                rows={5}
                maxCharacterCount={250}
                error={
                  formik.touched.payment_terms && formik.errors.payment_terms
                }
              />
            </div>
            <div className={Styles.box1}>
              <h4>Other Details</h4>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Select
                  name="vendor_category_id"
                  label="Vendor Category Type"
                  defaultLabel="Select from options"
                  mandatory={true}
                  width="250px"
                  value={formik.values.vendor_category_id}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.vendor_category_id &&
                    formik.errors.vendor_category_id
                  }
                >
                  {getAllVendorTypeList?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_id}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div>
                <Input
                  name="lead_time"
                  label="Delivery Duration"
                  placeholder="Enter Delivery Duration"
                  value={formik.values.lead_time}
                  onChange={formik.handleChange}
                  width="250px"
                  error={formik.touched.lead_time && formik.errors.lead_time}
                />
              </div>
              <div>
                <Input
                  name="minimum_order_quantity"
                  label="Minimum Order Quantity"
                  placeholder="Enter Minimum Order Quantity"
                  value={formik.values.minimum_order_quantity}
                  onChange={formik.handleChange}
                  width="250px"
                  error={
                    formik.touched.minimum_order_quantity &&
                    formik.errors.minimum_order_quantity
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRowLast}>
              <div>
                <Input
                  name="tax_id"
                  label="Tax Number"
                  mandatory={true}
                  placeholder="Enter tax number"
                  value={formik.values.tax_id}
                  onChange={formik.handleChange}
                  width="250px"
                  error={formik.touched.tax_id && formik.errors.tax_id}
                />
              </div>
            </div>
            <div className={Styles.fieldRowLast}>
              <TextArea
                name="notes"
                label="Notes"
                placeholder="Enter Notes"
                width="35%"
                value={formik.values.notes}
                onChange={formik.handleChange}
                rows={5}
                maxCharacterCount={250}
                error={formik.touched.notes && formik.errors.notes}
              />
            </div>
          </div>
          <div className={Styles.buttonFields}>
            <div>
              {projectId !== null ? (
                <Button
                  color="secondary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={() => {
                    navigate('/purchase-request-add', {
                      state: { project_id: projectId, indent_id: indentId },
                    });
                  }}
                >
                  Back
                </Button>
              ) : (
                <Button
                  color="secondary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  onClick={() => {
                    navigate('/settings');
                  }}
                >
                  Back
                </Button>
              )}
            </div>
            <div>
              {routeParams.id ? (
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  type="submit"
                >
                  Update
                </Button>
              ) : (
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                  type="submit"
                >
                  Save
                </Button>
              )}
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

export default AddVendor;
