import React, { useState, useEffect } from 'react';
import Styles from '../../styles/storeAdd.module.scss';
import { useNavigate } from 'react-router-dom';
import Input from '../ui/Input';
import Button from '../ui/Button';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import {
  getSitesByProjectId,
  useGetAllProjectDrop,
} from '../../hooks/project-hooks';
import { useParams } from 'react-router-dom';
import { store, RootState } from '../../redux/store';
import { getToken } from '../../redux/reducer';
import { getCreateValidateyup } from '../../helper/constants/store-constants';
import { useGetAllUsersDrop } from '../../hooks/user-hooks';
import { createStore, updateStore } from '../../hooks/store-hooks';
import CustomSnackBar from '../ui/customSnackBar';
import StoreService from '../../service/store-service';
import AddressComponenet from '../ui/customAddressComponent';

const AddStore = () => {
  const { mutate: createNewStore } = createStore();
  const { mutate: updateOneStore } = updateStore();
  const { data: getAllProjectList = [] } = useGetAllProjectDrop();
  const { data: getAllUsersDatadrop = [] } = useGetAllUsersDrop();
  const state: RootState = store.getState();
  const encryptedData = getToken(state, 'Data');
  const userData: any = encryptedData.userData;
  const routeParams = useParams();
  const navigate = useNavigate();
  const validationSchema = getCreateValidateyup(Yup);
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [disable, setDisable] = useState(
    routeParams?.id !== undefined ? true : false
  );
  const [initialValues, setInitialValues] = useState({
    store_id: '',
    store_name: '',
    store_manager_id: '',
    contact_email: '',
    contact_phone: '',
    project_id: '',
    site_id: '',
    address: {
      street: '',
      city: '',
      state: '',
      country: '',
      pin_code: '',
    },
    created_by: '',
  });

  useEffect(() => {
    if (Number(routeParams?.id)) {
      const fetchOne = async () => {
        const data = await StoreService.getOneStoreByID(
          Number(routeParams?.id)
        );
        console.log('data', data);
        setInitialValues({
          store_id: data?.data?.store_id,
          store_name: data?.data?.store_name,
          store_manager_id: data?.data?.store_manager_id,
          contact_email: data?.data?.contact_email,
          contact_phone: data?.data?.contact_phone,
          project_id: data?.data?.project_id,
          site_id: data?.data?.site_id,
          created_by: data?.data?.created_by,
          address: {
            street: data?.data?.address?.street || '',
            city: data?.data?.address?.city || '',
            state: data?.data?.address?.state || '',
            country: data?.data?.address?.country || '',
            pin_code: data?.data?.address?.pin_code || '',
          }
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
      if (Number(routeParams?.id)) {
        const Object: any = {
          store_id: values.store_id,
          store_name: values.store_name,
          store_manager_id: Number(values.store_manager_id),
          contact_email: values.contact_email,
          contact_phone: values.contact_phone,
          project_id: Number(values.project_id),
          site_id: Number(values.site_id),
          updated_by: userData.user_id,
          address: {
            street: values.address.street,
            city: values.address.city,
            state: values.address.state,
            country: values.address.country,
            pin_code: values.address.pin_code,
          },
        };
        updateOneStore(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Store Edited');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      } else {
        const Object: any = {
          store_name: values.store_name,
          store_manager_id: Number(values.store_manager_id),
          contact_email: values.contact_email,
          contact_phone: values.contact_phone,
          project_id: Number(values.project_id),
          site_id: Number(values.site_id),
          created_by: userData.user_id,
          address: {
            street: values.address.street,
            city: values.address.city,
            state: values.address.state,
            country: values.address.country,
            pin_code: values.address.pin_code,
          },
        };
        createNewStore(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Store created');
              setOpenSnack(true);
              setTimeout(() => {
                navigate('/settings');
              }, 1000);
            }
          },
        });
      }
    },
  });

  const { data: getAllSiteList = [] } = getSitesByProjectId(
    Number(formik.values.project_id)
  );

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  return (
    <div className={Styles.formContainer}>
      <div className={Styles.box}>
        <div>
          <h3>{routeParams.id ? 'Store Edit' : 'Store Add'}</h3>
        </div>
      </div>
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.form}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.formFields}>
            <div className={Styles.fieldRow}>
              <div style={{ width: '20%' }}>
                <Input
                  name="store_name"
                  label="Store Name"
                  placeholder="Enter store name"
                  mandatory={true}
                  value={formik.values.store_name}
                  onChange={formik.handleChange}
                  error={formik.touched.store_name && formik.errors.store_name}
                />
              </div>
              <div style={{ width: '20%' }}>
                <AutoCompleteSelect
                  name="store_manager_id"
                  label="Store Manager Name"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  value={formik.values.store_manager_id}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.store_manager_id &&
                    formik.errors.store_manager_id
                  }
                  onSelect={(value) => {
                    formik.setFieldValue('store_manager_id', value);
                  }}
                  optionList={getAllUsersDatadrop}
                />
              </div>
              <div style={{ width: '20%' }}>
                <Input
                  name="contact_email"
                  label="Store Email"
                  placeholder="Enter store email"
                  mandatory={true}
                  value={formik.values.contact_email}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.contact_email && formik.errors.contact_email
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div style={{ width: '20%' }}>
                <Input
                  name="contact_phone"
                  label="Store Phone Number"
                  placeholder="Enter store phone no"
                  mandatory={true}
                  value={formik.values.contact_phone}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.contact_phone && formik.errors.contact_phone
                  }
                />
              </div>
              <div style={{ width: '20%' }}>
                <AutoCompleteSelect
                  name="project_id"
                  label="Project"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  mandatory={true}
                  value={formik.values.project_id}
                  onChange={formik.handleChange}
                  error={formik.touched.project_id && formik.errors.project_id}
                  onSelect={(value) => {
                    formik.setFieldValue('project_id', value);
                  }}
                  optionList={getAllProjectList}
                  disabled={disable}
                />
              </div>
              <div style={{ width: '20%' }}>
                <AutoCompleteSelect
                  name="site_id"
                  label="Site"
                  defaultLabel="Select from options"
                  placeholder="Select from options"
                  value={formik.values.site_id}
                  onChange={formik.handleChange}
                  error={formik.touched.project_id && formik.errors.site_id}
                  onSelect={(value) => {
                    formik.setFieldValue('site_id', value);
                  }}
                  optionList={getAllSiteList}
                  disabled = {formik.values.project_id !== '' ?  disable : ''}
                />
              </div>
            </div>
            <AddressComponenet formik={formik} />
            <div className={Styles.buttonFields}>
              <div>
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
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                >
                  Save
                </Button>
              </div>
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
export default AddStore;
