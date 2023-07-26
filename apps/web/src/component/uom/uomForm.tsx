import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { Grid } from '@mui/material';
import { createuom, updateUom } from '../../hooks/uom-hooks';
import {
  getuomCreateValidateyup,
  getuomUpdateValidateyup,
} from '../../helper/constants/uom-constants';
import uomService from '../../service/uom-service';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
const UomForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'ADD'
      ? getuomCreateValidateyup(Yup)
      : getuomUpdateValidateyup(Yup);
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
  const { mutate: createNewuom } = createuom();
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
          spacing={{ xs: 1, md: 1 }}
          columns={{ xs: 4, sm: 8, md: 12 }}
        >
          <Grid item xs={2} sm={4} md={12}>
            <Input
              name="name"
              label="Unit Of Measurement"
              placeholder="Enter unit of measurement"
              value={formik.values.name}
              onChange={formik.handleChange}
              error={formik.touched.name && formik.errors.name}
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
             <Input
              name="description"
              label="Description"
              placeholder="Enter description"
              value={formik.values.description}
              onChange={formik.handleChange}
              error={formik.touched.description && formik.errors.description}
            />
          </Grid>

          <Grid item xs={2} sm={4} md={6}>
            <Button color="primary" shape="rectangle" justify="center">
              Submit
            </Button>
          </Grid>
        </Grid>
      </form>
    </div>
  );
};

export default UomForm;
