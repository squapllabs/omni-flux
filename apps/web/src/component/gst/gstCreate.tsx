import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getGstcreationYupschema } from '../../helper/constants/gst-constants';
import { useCreateGst, useUpdateGst } from '../../hooks/gst-hooks';
import gstService from '../../service/gst-service';
import Input from '../ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/gstList.module.scss';

const validationSchema = getGstcreationYupschema(Yup);

/* Function fOR GST Form */
const GstCreate: React.FC = (props: any) => {
  const { mutate: createNewGst } = useCreateGst();
  const { mutate: updateGstById } = useUpdateGst();
  const [initialValues, setInitialValues] = useState({
    gst_id: '',
    rate: '',
  });

  useEffect(() => {
    if (props?.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await gstService.getOneGst(props?.gstId);
        setInitialValues({
          gst_id: data?.data?.gst_id,
          rate: data?.data?.rate,
        });
      };
      fetchOne();
    }
  }, []);

  // Function for updating and adding gst form data
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
        const Object: any = {
          gst_id: values.gst_id,
          rate: parseFloat(values.rate),
        };
        createNewGst(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              props.setOpen(false);
              props.setMessage('Gst created successfully');
              props.setOpenSnack(true);
            }
          },
        });
      } else {
        const Object: any = {
          gst_id: values.gst_id,
          rate: parseFloat(values.rate),
        };
        updateGstById(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Gst edited successfully');
              props.setOpenSnack(true);
            }
          },
        });
      }
    },
  });

  //Function for closiing popup
  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div >
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.field}>
          <Input
            label="Gst Rate"
            placeholder="Enter gst rate"
            name="rate"
            mandatory={true}
            value={formik.values.rate}
            onChange={formik.handleChange}
            error={formik.touched.rate && formik.errors.rate}
            width="350px"
          />
        </div>
        <div className={Styles.footer}>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.formButton}>
            <div>
              <Button className={Styles.cancelButton} shape="rectangle" justify="center" size="small" onClick={handleClose}>
                Cancel
              </Button>
            </div>
            <div>
              <Button color="primary" shape="rectangle" justify="center" size="small" type="submit">
                Submit
              </Button>
            </div>
          </div>
        </div>
      </form>
    </div >
  );
};

export default GstCreate;
