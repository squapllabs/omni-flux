import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { Grid } from '@mui/material';
import { createHsnCode, updateHsnCode } from '../../hooks/hsnCode-hooks';
import {
  gethsnCreateValidateyup,
  gethsnUpdateValidateyup,
} from '../../helper/constants/hsn-constants';
import hsnCodeService from '../../service/hsnCode-service';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../menu/button';

const HsnCodeForm: React.FC = (props: any, { mode, id }) => {
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
  const validationSchema =
    props.mode === 'ADD'
      ? gethsnCreateValidateyup(Yup)
      : gethsnUpdateValidateyup(Yup);
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
            <Input
              label="Code"
              placeholder="Enter product code"
              name="code"
              value={formik.values.code}
              onChange={formik.handleChange}
              error={formik.touched.code && formik.errors.code}
              width="100%"
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Input
              label="Description"
              placeholder="Enter product description"
              name="description"
              value={formik.values.description}
              onChange={formik.handleChange}
              error={formik.touched.description && formik.errors.description}
              width="100%"
            />
          </Grid>
          <Grid item xs={2} sm={4} md={6}>
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

export default HsnCodeForm;
