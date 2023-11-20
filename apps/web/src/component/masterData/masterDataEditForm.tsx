import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { useUpdatemasterData } from '../../hooks/masertData-hook';
import { getUpdateValidateyup } from '../../helper/constants/master-constants';
import MasterService from '../../service/masterData-service';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import TextArea from '../ui/CustomTextArea';
import Styles from '../../styles/masterdata.module.scss';

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
  const { mutate: updateMasterData } = useUpdatemasterData();

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
            props.setMessage('Master Data edited');
            props.setOpenSnack(true);
          }
        },
      });
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.header}>
          <div>
            <h4 className={Styles.titleStyle}>Edit Master Data</h4>
          </div>
          <div>
            <CancelIcon onClick={handleClose} />
          </div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Input
            name="master_data_name"
            label="Name"
            placeholder="Enter name"
            value={formik.values.master_data_name}
            onChange={formik.handleChange}
            error={
              formik.touched.master_data_name && formik.errors.master_data_name
            }
            width="100%"
          />
        </div>
        <div className={Styles.field}>
          <TextArea
            name="master_data_description"
            label="Description"
            placeholder="Enter Description"
            value={formik.values.master_data_description}
            onChange={formik.handleChange}
            error={
              formik.touched.master_data_description &&
              formik.errors.master_data_description
            }
            rows={3}
            maxCharacterCount={100}
          />
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.formButton}>
          <div>
            <Button
              className={Styles.cancelButton}
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleClose}
            >
              Cancel
            </Button>
          </div>
          <div>
            <Button
              type="submit"
              color="primary"
              shape="rectangle"
              justify="center"
              size="small"
            >
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default MasterDataEditForm;
