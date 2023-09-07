import React, { useState } from 'react'
import Styles from '../../styles/labourAdd.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Button from '../ui/Button';
import AddIcon from '../menu/icons/addIcon';
import EditIcon from '../menu/icons/editIcon';
import SearchIcon from '../menu/icons/search';
import { useNavigate } from 'react-router-dom';

const LabourAddForm = () => {

  const [initialValues, setInitialValues] = useState({
    labour_id: '',
    labour_type: '',
    uom_id: '',
    rate: '',
  });
  const navigate = useNavigate();
  //   const validationSchema = getCreateValidateyup(Yup);
  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        console.log("values", values);
      }
    }
  });

  return (
    <div >
      <div className={Styles.box}>
        <div>
          <h3>Labour Add/Edit</h3>
        </div>
      </div>
      <div className={Styles.dividerStyle}></div>
      <div className={Styles.form}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.formFields}>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  name="labour_type"
                  label="Labour Type"
                  placeholder="Enter Labour Type"
                  value={formik.values.labour_type}
                  onChange={formik.handleChange}
                  mandatory={true}
                  width="250px"
                  error={
                    formik.touched.labour_type &&
                    formik.errors.labour_type
                  }
                />
              </div>

              <div>
                <AutoCompleteSelect
                  label="UOM Type"
                  name="uom_id"
                  onChange={formik.handleChange}
                  value={formik.values.uom_id}
                  placeholder="Select from options"
                  width="250px"
                  onSelect={(value) => {
                    formik.setFieldValue('uom_id', value);
                  }}
                  // optionList={
                  //     dropLoading === true ? [] : getAllmasterDataForDrop
                  // }
                  error={
                    formik.touched.uom_id &&
                    formik.errors.uom_id
                  }
                />
              </div>
            </div>
            <div className={Styles.fieldRow}>
              <div>
                <Input
                  label="Rate"
                  placeholder="Enter Rate"
                  name="rate"
                  width="250px"
                  onChange={formik.handleChange}
                  value={formik.values.rate}
                  error={formik.touched.rate && formik.errors.rate}
                />
              </div>
              
              </div>
              <div className={Styles.buttonFields}>
                <div>
                  <Button
                    color="secondary"
                    shape="rectangle"
                    justify="center"
                    size="small"
                    onClick={() => {
                      navigate('/labour');
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
      {/* <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      /> */}
    </div>

  )
}
export default LabourAddForm;
