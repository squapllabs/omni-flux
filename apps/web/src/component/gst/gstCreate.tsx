import React, { useState ,useEffect} from 'react';
import Customs from '../ui/custom';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getGstcreationYupschema } from '../../helper/constants/gst-constants';
import { Grid } from '@mui/material';
import { createGst,updateGst } from '../../hooks/gst-hooks';
import gstService from '../../service/gst-service';

const validationSchema = getGstcreationYupschema(Yup);

const GstCreate: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    gst_id:'',
    rate: '',
    cgst_rate: '',
    igst_rate: '',
  });
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await gstService.getOneGst(props.gstId);
        setInitialValues({
          gst_id:data?.data?.gst_id,
          rate: data?.data?.rate,
          cgst_rate: data?.data?.cgst_rate,
          igst_rate: data?.data?.igst_rate,
        });
      };
      fetchOne();
    }
  }, []);

  const { mutate: createNewGst } = createGst();
  const { mutate: updateGstById } = updateGst();

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
      const Object: any = {
        gst_id:values.gst_id,
        rate: parseFloat(values.rate),
        cgst_rate: parseFloat(values.cgst_rate),
        igst_rate: parseFloat(values.igst_rate),
      };
      createNewGst(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success) {
            props.setOpenPopup(false);
            props.setReload(true);
            props.setMessage('Gst created successfully');
            props.setOpenDeleteSnack(true);
          }
        }
      });
    }
    else {
      const Object: any = {
        gst_id:values.gst_id,
        rate: parseFloat(values.rate),
        cgst_rate: parseFloat(values.cgst_rate),
        igst_rate: parseFloat(values.igst_rate),
      };
      updateGstById(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.success) {
            props.setOpenPopup(false);
            props.setReload(true);
            props.setMessage('Gst edited successfully');
            props.setOpenDeleteSnack(true);
          }
        },
      });
    }
    },
  });
  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <Grid
          container
          spacing={{ xs: 2, md: 3 }}
          columns={{ xs: 4, sm: 8, md: 12 }}
        >
          <Grid item xs={2} sm={4} md={4}>
            <Customs.CustomTextField
              name="rate"
              label="Gst Rate"
              variant="outlined"
              size="small"
              value={formik.values.rate}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={formik.touched.rate && Boolean(formik.errors.rate)}
              helperText={formik.touched.rate && formik.errors.rate}
            />
          </Grid>

          <Grid item xs={2} sm={4} md={4}>
            <Customs.CustomTextField
              name="cgst_rate"
              label="Cgst Rate"
              variant="outlined"
              size="small"
              value={formik.values.cgst_rate}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={
                formik.touched.cgst_rate && Boolean(formik.errors.cgst_rate)
              }
              helperText={formik.touched.cgst_rate && formik.errors.cgst_rate}
            />
          </Grid>

          <Grid item xs={2} sm={4} md={4}>
            <Customs.CustomTextField
              name="igst_rate"
              label="Igst Rate"
              variant="outlined"
              size="small"
              value={formik.values.igst_rate}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={
                formik.touched.igst_rate && Boolean(formik.errors.igst_rate)
              }
              helperText={formik.touched.igst_rate && formik.errors.igst_rate}
            />
          </Grid>

          <Grid item xs={2} sm={4} md={12}>
            <Customs.CustomButton
              type="submit"
              label="Submit"
              variant="outlined"
              color="primary"
            />
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default GstCreate;
