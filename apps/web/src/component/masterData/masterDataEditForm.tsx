import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { Grid, InputLabel } from '@mui/material';
import { updatemasertData } from '../../hooks/masertData-hook';
import { getUpdateValidateyup } from '../../helper/constants/master-constants';
import MasterService from '../../service/masterData-service';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';

const MasterDataEditForm: React.FC = (props: any) => {
  const validationSchema = getUpdateValidateyup(Yup);
  const [initialValues, setInitialValues] = useState({
    master_data_id: '',
    master_data_name: '',
    master_data_description: '',
  });
  useEffect(() => {
    const fetchOne = async () => {
      const data = await MasterService.getOnemasertDataByID(props.masterID);
      setInitialValues({
        master_data_id: data?.data?.master_data_id,
        master_data_name: data?.data?.master_data_name,
        master_data_description: data?.data?.master_data_description,
      });
    };
    fetchOne();
  }, []);
  const { mutate: updateMasterData } = updatemasertData();

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values) => {
      const Object: any = {
        master_data_id: values.master_data_id,
        master_data_name: values.master_data_name,
        master_data_description: values.master_data_description,
      };
      updateMasterData(Object, {
        onSuccess: (data, variables, context) => {
          if (data?.message === 'success') {
            props.setOpen(false);
            props.setReload(true);
            props.setMessage('masterData edited');
            props.setOpenSnack(true);
          }
        },
      });
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
              name="master_data_name"
              label="Name"
              placeholder="Enter name"
              value={formik.values.master_data_name}
              onChange={formik.handleChange}
              error={
                formik.touched.master_data_name &&
                formik.errors.master_data_name
              }
            />
          </Grid>
          <Grid item xs={2} sm={4} md={12}>
            <Input
              name="master_data_description"
              label="Description"
              placeholder="Enter Description"
              value={formik.values.master_data_description}
              onChange={formik.handleChange}
              error={
                formik.touched.master_data_description &&
                formik.errors.master_data_description
              }
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

export default MasterDataEditForm;
