import React, { useState } from 'react';
import Styles from '../../styles/labourAdd.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Button from '../ui/Button';
// import CustomSnackBar from '../ui/customSnackBar';
// import { useNavigate } from 'react-router-dom';
import * as Yup from 'yup';
import {
  useGetLabourUomForDrop,
  useCreateInstantLabour,
} from '../../hooks/labour-hooks';
import { getLabourCreationYupschema } from '../../helper/constants/labour-constants';

const InstantLabourAdd = (props: {
  isVissible: any;
  onAction: any;
  setMessage: any;
  setOpenSnack: any;
}) => {
  const { onAction, setMessage, setOpenSnack } = props;
  const [initialValues, setInitialValues] = useState({
    labour_id: '',
    labour_type: '',
    uom_id: '',
    rate: '',
  });
  const { mutate: createNewLabour } = useCreateInstantLabour();
  const { data: getLaboursUom = [], isLoading: dropLoading } =
    useGetLabourUomForDrop();
  const validationSchema = getLabourCreationYupschema(Yup);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      const Object: any = {
        labour_type: values.labour_type,
        uom_id: values.uom_id,
        rate: Number(values.rate),
      };
      createNewLabour(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            setMessage('Labour created');
            setOpenSnack(true);
            handleCloseForm();
            resetForm();
          }
        },
      });
    },
  });
  const handleCloseForm = () => {
    onAction(false);
    formik.resetForm();
  };
  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.formFields}>
          <div>
            <Input
              name="labour_type"
              label="Labour Type"
              placeholder="Enter Labour Type"
              value={formik.values.labour_type}
              onChange={formik.handleChange}
              mandatory={true}
              width="250px"
              error={formik.touched.labour_type && formik.errors.labour_type}
            />
          </div>
          <div>
            <AutoCompleteSelect
              label="UOM Type"
              name="uom_id"
              onChange={formik.handleChange}
              value={formik.values.uom_id}
              placeholder="Select from options"
              mandatory={true}
              width="250px"
              onSelect={(value) => {
                formik.setFieldValue('uom_id', value);
              }}
              optionList={dropLoading === true ? [] : getLaboursUom}
              error={formik.touched.uom_id && formik.errors.uom_id}
            />
          </div>
          <div>
            <Input
              label="Rate"
              placeholder="Enter Rate"
              name="rate"
              width="250px"
              onChange={formik.handleChange}
              mandatory={true}
              value={formik.values.rate}
              error={formik.touched.rate && formik.errors.rate}
            />
          </div>
        </div>
        <div className={Styles.footer}>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.submitButton}>
            <div>
              <Button
                className={Styles.cancelButton}
                color="secondary"
                shape="rectangle"
                justify="center"
                size="small"
                onClick={() => {
                  handleCloseForm();
                }}
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
              >
                Save
              </Button>
            </div>
          </div>
        </div>
      </form>
    </div>
  );
};

export default InstantLabourAdd;
