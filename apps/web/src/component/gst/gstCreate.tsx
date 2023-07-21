import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import * as Yup from 'yup';
import { getGstcreationYupschema } from '../../helper/constants/gst-constants';
import { Grid } from '@mui/material';
import { createGst, updateGst } from '../../hooks/gst-hooks';
import gstService from '../../service/gst-service';
import Input from '../ui/Input';
import Button from '../menu/button';

const validationSchema = getGstcreationYupschema(Yup);

const GstCreate: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    gst_id: '',
    rate: '',
    cgst_rate: '',
    igst_rate: '',
    sgst_rate: '',
  });
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await gstService.getOneGst(props.gstId);
        setInitialValues({
          gst_id: data?.data?.gst_id,
          rate: data?.data?.rate,
          cgst_rate: data?.data?.cgst_rate,
          igst_rate: data?.data?.igst_rate,
          sgst_rate: data?.data?.sgst_rate,
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
          gst_id: values.gst_id,
          rate: parseFloat(values.rate),
          cgst_rate: parseFloat(values.cgst_rate)
            ? parseFloat(values.cgst_rate)
            : 0,
          igst_rate: parseFloat(values.igst_rate)
            ? parseFloat(values.igst_rate)
            : 0,
          sgst_rate: parseFloat(values.sgst_rate)
            ? parseFloat(values.sgst_rate)
            : 0,
        };
        createNewGst(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpenPopup(false);
              props.setReload(true);
              props.setMessage('Gst created successfully');
              props.setOpenDeleteSnack(true);
            }
          },
        });
      } else {
        const Object: any = {
          gst_id: values.gst_id,
          rate: parseFloat(values.rate),
          cgst_rate: parseFloat(values.cgst_rate),
          igst_rate: parseFloat(values.igst_rate),
          sgst_rate: parseFloat(values.sgst_rate),
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
            <Input
              label="Gst Rate"
              placeholder="Enter gst rate"
              name="rate"
              value={formik.values.rate}
              onChange={formik.handleChange}
              error={formik.touched.rate && formik.errors.rate}
              width="100%"
            />
          </Grid>

          <Grid item xs={2} sm={4} md={4}>
            <Input
              label="Sgst Rate"
              placeholder="Enter sgst rate"
              name="sgst_rate"
              value={formik.values.sgst_rate}
              onChange={formik.handleChange}
              error={formik.touched.sgst_rate && formik.errors.sgst_rate}
              width="100%"
            />
          </Grid>

          <Grid item xs={2} sm={4} md={4}>
            <Input
              label="Cgst Rate"
              placeholder="Enter cgst rate"
              name="cgst_rate"
              value={formik.values.cgst_rate}
              onChange={formik.handleChange}
              error={formik.touched.cgst_rate && formik.errors.cgst_rate}
              width="100%"
            />
          </Grid>

          <Grid item xs={2} sm={4} md={4}>
            <Input
              label="Igst Rate"
              placeholder="Enter igst rate"
              name="igst_rate"
              value={formik.values.igst_rate}
              onChange={formik.handleChange}
              error={formik.touched.igst_rate && formik.errors.igst_rate}
              width="100%"
            />
          </Grid>

          <Grid item xs={2} sm={4} md={12}>
            <Button
              text="Submit"
              backgroundColor="#7F56D9"
              fontSize={14}
              fontWeight={500}
              width={125}
            />
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default GstCreate;
