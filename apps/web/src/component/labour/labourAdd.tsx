import React, { useState, useEffect } from 'react'
import Styles from '../../styles/labourAdd.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import * as Yup from 'yup';
import {
  useGetLabourUomForDrop,
  useCreateLabour,
  useUpdateLabour
} from '../../hooks/labour-hooks';
import LabourService from '../../service/labour-service';
import {
  getLabourCreationYupschema,
  getLabourUpdateYupschema
} from '../../helper/constants/labour-constants';

/* Function for labour */
const LabourAddForm = (props : any) => {  

  const [initialValues, setInitialValues] = useState({
    labour_id: '',
    labour_type: '',
    uom_id: '',
    rate: '',
  });
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const { mutate: createNewLabour } = useCreateLabour();
  const { mutate: updateOneLabour } = useUpdateLabour();
  const { data: getLaboursUom = [], isLoading: dropLoading } = useGetLabourUomForDrop();

  /* Function to get one labour data by ID */
  useEffect(() => {
    if (props?.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await LabourService.getOneLabourID(
          Number(props?.labourId)
        );
        setInitialValues({
          labour_id: data?.data?.labour_id,
          labour_type: data?.data?.labour_type,
          uom_id: data?.data?.uom_id,
          rate: data?.data?.rate,
        });
      };
      fetchOne();
    }
  }, []);

  const handleClose = () => {
    props.setOpen(false);
};

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const validationSchema =
   ( props?.labourId === undefined ||  props?.labourId === "")
      ? getLabourCreationYupschema(Yup)
      : getLabourUpdateYupschema(Yup);

  /* Function to create and edit labour data */
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (Number(props?.labourId)) {
        const Object: any = {
          labour_id: values.labour_id,
          labour_type: values.labour_type,
          uom_id: values.uom_id,
          rate: Number(values.rate)
        };
        updateOneLabour(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Labour Edited');
              setOpenSnack(true);
              setTimeout(() => {
                handleClose()
              }, 1000);
              resetForm()
            }
          },
        });
      }
      else {
        const Object: any = {
          labour_type: values.labour_type,
          uom_id: values.uom_id,
          rate: Number(values.rate)
        };
        createNewLabour(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              setMessage('Labour created');
              setOpenSnack(true);
              setTimeout(() => {
                handleClose()
              }, 1000);
              resetForm()
            }
          },
        });
      }
    }
  });

  return (
    <div >
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
              mandatory={true}
              width="250px"
              onSelect={(value) => {
                formik.setFieldValue('uom_id', value);
              }}
              optionList={
                dropLoading === true ? [] : getLaboursUom
              }
              error={
                formik.touched.uom_id &&
                formik.errors.uom_id
              }
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
                  handleClose()
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
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>

  )
}
export default LabourAddForm;
