import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import Customs from '../ui/custom';
import { Grid, InputLabel, TextareaAutosize } from '@mui/material';
import { createuom, updateUom } from '../../hooks/uom-hooks';
import { getuomValidateyup } from '../../helper/constants/uom-constants';
import uomService from '../../service/uom-service';
import * as Yup from 'yup';
const validationSchema = getuomValidateyup(Yup);
const uomForm: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    uom_id: '',
    name: '',
    description: '',
  });
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await uomService.getOneUomByID(props.uomId);
        setInitialValues({
          uom_id: data?.data?.uom_id,
          name: data?.data?.name,
          description: data?.data?.description,
        });
      };

      fetchOne();
    }
  }, []);
  const { mutate: createNewuom, isLoading } = createuom();
  const { mutate: updateuom } = updateUom();
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      if (props.mode === 'ADD') {
        const Object: any = {
          name: values.name,
          description: values.description,
        };
        createNewuom(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('UOM created');
              props.setOpenSnack(true);
            } else {
            }
          },
        });
      } else {
        const Object: any = {
          uom_id: values.uom_id,
          name: values.name,
          description: values.description,
        };
        updateuom(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.success) {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('UOM edited');
              props.setOpenSnack(true);
            } else {
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
              name="name"
              label="Name"
              variant="outlined"
              size="small"
              value={formik.values.name}
              onChange={formik.handleChange}
              onBlur={formik.handleBlur}
              error={formik.touched.name && Boolean(formik.errors.name)}
              helperText={formik.touched.name && formik.errors.name}
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

export default uomForm;
