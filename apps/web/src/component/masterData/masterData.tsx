import React, { useState, useEffect } from 'react';
import Styles from '../../styles/masterdata.module.scss';
import Select from '../ui/Select';
import Input from '../ui/Input';
import Button from '../ui/Button';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getCreateValidateyup } from '../../helper/constants/master-constants';
import MySnackbar from '../ui/MySnackbar';
const MaterData = () => {
  const [selectedValue, setSelectedValue] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [initialValues, setInitialValues] = useState({
    master_data_id: '',
    master_data_name: '',
    master_data_description: '',
    master_data_type: '',
    parent_master_data_id: '',
  });
  const [parentId, setparentId] = useState([
    {
      label: 'Tamil',
      value: 'T',
    },
  ]);
  const handleDropdownChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ) => {
    const selectedRoleId = event.target.value;
    setSelectedValue(selectedRoleId);
  };
  const validationSchema = getCreateValidateyup(Yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        console.log('values', values);
        setMessage('Master Data has added successfully');
        setOpenSnack(true);
        setSelectedValue('');
        resetForm();
      }
    },
  });
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  return (
    <div>
      <div className={Styles.conatiner}>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>Add New Master Data</h3>
            <span className={Styles.content}>
              Manage your raw materials (Raw, Semi Furnished & Finished).
            </span>
          </div>
          <form onSubmit={formik.handleSubmit}>
            <div className={Styles.fields_container}>
              <div>
                <Input
                  name="master_data_name"
                  label="Name"
                  placeholder="Enter master name"
                  value={formik.values.master_data_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.master_data_name &&
                    formik.errors.master_data_name
                  }
                />
              </div>
              <div>
                <Input
                  name="master_data_description"
                  label="Description"
                  placeholder="Enter description"
                  value={formik.values.master_data_description}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.master_data_description &&
                    formik.errors.master_data_description
                  }
                />
              </div>
              <div>
                <Input
                  name="master_data_type"
                  label="Code"
                  placeholder="Enter code"
                  value={formik.values.master_data_type}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.master_data_type &&
                    formik.errors.master_data_type
                  }
                />
              </div>
              <div className={Styles.projectField}>
                <span className={Styles.projectHeading}>Matser</span>
                <Select
                  options={parentId}
                  onChange={handleDropdownChange}
                  value={selectedValue}
                  defaultLabel="Select from options"
                />
                {formik.touched.parent_master_data_id &&
                  formik.errors.parent_master_data_id && (
                    <div className={Styles.error}>
                      {formik.errors.parent_master_data_id}
                    </div>
                  )}
              </div>
              <div>
                <Button
                  color="primary"
                  shape="rectangle"
                  justify="center"
                  size="small"
                >
                  Add new Master Data
                </Button>
              </div>
            </div>
          </form>
        </div>
        <div className={Styles.box}>
          <div className={Styles.textContent}>
            <h3>List of Master Data</h3>
            <span className={Styles.content}>
              Manage your raw materials (Raw, Semi Furnished & Finished).
            </span>
          </div>
        </div>
      </div>
      <MySnackbar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        severity={'success'}
        autoHideDuration={1000}
      />
    </div>
  );
};

export default MaterData;
