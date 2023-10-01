import React, { useState, useEffect } from 'react'
import Styles from '../../styles/labourAdd.module.scss';
import { useFormik } from 'formik';
import Input from '../ui/Input';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';
import Button from '../ui/Button';
import CustomSnackBar from '../ui/customSnackBar';
import { useNavigate } from 'react-router-dom';
import * as Yup from 'yup';
import {
  useGetLabourUomForDrop,
  createLabour,
  updateLabour
} from '../../hooks/labour-hooks';
import { useParams } from 'react-router-dom';
import LabourService from '../../service/labour-service';
import {
  getLabourCreationYupschema,
  getLabourUpdateYupschema
} from '../../helper/constants/labour-constants';

const LabourAddForm = () => {

  const [initialValues, setInitialValues] = useState({
    labour_id: '',
    labour_type: '',
    uom_id: '',
    rate: '',
  });
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const navigate = useNavigate();
  const routeParams = useParams();
  const { mutate: createNewLabour } = createLabour();
  const { mutate: updateOneLabour } = updateLabour();
  const { data: getLaboursUom = [], isLoading: dropLoading } = useGetLabourUomForDrop();



  useEffect(() => {
    if (Number(routeParams?.id)) {
      const fetchOne = async () => {
        const data = await LabourService.getOneLabourID(
          Number(routeParams?.id)
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
  }, [routeParams?.id]);

  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };

  const validationSchema =
    routeParams?.id === undefined
      ? getLabourCreationYupschema(Yup)
      : getLabourUpdateYupschema(Yup);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (Number(routeParams?.id)) {
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
                navigate('/labour');
              }, 1000);
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
                navigate('/labour');
              }, 1000);
            }
          },
        });
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
            </div>
            <div className={Styles.fieldRow}>
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

  )
}
export default LabourAddForm;
