import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import Customs from '../ui/custom';
import { Grid, InputLabel, TextareaAutosize } from '@mui/material';
import { createHsnCode, updateHsnCode } from '../../hooks/hsnCode-hooks';
import { gethisnValidateyup } from '../../helper/constants/hsn-constants';
import hsnCodeService from '../../service/hsnCode-service';
import * as Yup from 'yup';
const validationSchema = gethisnValidateyup(Yup);

const HsnCodeForm: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    hsn_code_id: '',
    code: '',
    description: '',
  });
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await hsnCodeService.getOneHsnCode(props.hsnCodeId);
        setInitialValues({
          hsn_code_id: data?.data?.hsn_code_id,
          code: data?.data?.code,
          description: data?.data?.description,
        });
      };

      fetchOne();
    }
  }, []);
  const { mutate: createNewHsnCode } = createHsnCode();
  const { mutate: updateHsnById } = updateHsnCode();
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
        const Object: any = {
          code: values.code,
          description: values.description,
        };
        createNewHsnCode(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpenPopup(false);
              props.setReload(true);
              props.setMessage('Hsc Code created');
              props.setOpenSnack(true);
            }
          },
        });
      } else {
        const Object: any = {
          hsn_code_id: values.hsn_code_id,
          code: values.code,
          description: values.description,
        };

        updateHsnById(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpenPopup(false);
              props.setReload(true);
              props.setMessage('Hsn Code edited');
              props.setOpenSnack(true);
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
          <Grid item xs={2} sm={4} md={12}>
            <Customs.CustomTextField
              name="code"
              label="Code"
              variant="outlined"
              size="small"
              value={formik.values.code}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={formik.touched.code && Boolean(formik.errors.code)}
              helperText={formik.touched.code && formik.errors.code}
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <InputLabel id="description_id">Description</InputLabel>
            <TextareaAutosize
              name="description"
              labelId="description_id"
              variant="outlined"
              minRows={4}
              style={{ width: '548px' }}
              fullWidth
              value={formik.values.description}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
            />
            {formik.errors.description && formik.touched.description && (
              <div style={{ color: 'red' }}>{formik.errors.description}</div>
            )}
          </Grid>
          <Grid item xs={2} sm={4} md={6}>
            <Customs.CustomButton
              type="submit"
              label="Submit"
              variant="contained"
              color="primary"
            />
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default HsnCodeForm;
