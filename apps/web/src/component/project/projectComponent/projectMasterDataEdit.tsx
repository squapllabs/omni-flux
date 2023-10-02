import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import {
  updatemasertData,
  createmasertData,
} from '../../../hooks/masertData-hook';
import {
  getUpdateValidateyup,
  getCreateValidateyup,
} from '../../../helper/constants/master-constants';
import MasterService from '../../../service/masterData-service';
import * as Yup from 'yup';
import Input from '../../ui/Input';
import Button from '../../ui/Button';
import CancelIcon from '../../menu/icons/closeIcon';
import TextArea from '../../ui/CustomTextArea';
import Styles from '../../../styles/masterdata.module.scss';

const ProjectMasterDataEditForm: React.FC = (props: any) => {
  const validationSchema =
    props.mode === 'EDIT'
      ? getUpdateValidateyup(Yup)
      : getCreateValidateyup(Yup);

  const [initialValues, setInitialValues] = useState({
    master_data_id: '',
    master_data_name: '',
    master_data_type: '',
    master_data_description: '',
    project_id: '',
  });
  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await MasterService.getOnemasertDataByID(props.masterID);
        setInitialValues({
          master_data_id: data?.data?.master_data_id,
          master_data_name: data?.data?.master_data_name,
          master_data_type: data?.data?.master_data_type,
          master_data_description: data?.data?.master_data_description,
          project_id: data?.data?.project_id,
        });
      };
      fetchOne();
    }
  }, []);
  const { mutate: postMasterData } = createmasertData();
  const { mutate: updateMasterData } = updatemasertData();

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (props.mode === 'EDIT') {
        const Object: any = {
          master_data_id: values.master_data_id,
          master_data_name: values.master_data_name,
          master_data_description: values.master_data_description,
        };
        updateMasterData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              props.setOpen(false);
              props.setMessage('Master Data edited');
              props.setOpenSnack(true);
              resetForm();
            }
          },
        });
      } else {
        const Object: any = {
          master_data_name: values.master_data_name,
          master_data_description: values.master_data_description,
          master_data_type: values.master_data_type,
          project_id: props.projectId,
        };
        postMasterData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              props.setOpen(false);
              props.setMessage('Master Data created');
              props.setOpenSnack(true);
              resetForm();
            }
          },
        });
      }
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  };

  return (
    <div>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.divOne}>
          <div style={{ width: '60%' }}>
            <div className={Styles.field}>
              <Input
                name="master_data_name"
                label="Name"
                placeholder="Enter name"
                value={formik.values.master_data_name}
                onChange={formik.handleChange}
                mandatory
                error={
                  formik.touched.master_data_name &&
                  formik.errors.master_data_name
                }
                width="185%"
              />
            </div>
            <div className={Styles.field}>
              <Input
                name="master_data_type"
                label="code"
                placeholder="Enter code"
                value={formik.values.master_data_type}
                onChange={formik.handleChange}
                mandatory
                error={
                  formik.touched.master_data_type &&
                  formik.errors.master_data_type
                }
                width="185%"
              />
            </div>
            <div className={Styles.field}>
              <TextArea
                name="master_data_description"
                label="Description"
                placeholder="Enter Description"
                value={formik.values.master_data_description}
                onChange={formik.handleChange}
                mandatory
                error={
                  formik.touched.master_data_description &&
                  formik.errors.master_data_description
                }
                rows={3}
                maxCharacterCount={100}
                width="120%"
              />
            </div>
          </div>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
            }}
          >
            <img src="/masterDataImage.png" alt="aa" width="75%" height="75%" />
          </div>
        </div>
        <div className={Styles.footer}>
          <div className={Styles.dividerStyle}></div>
          <div className={Styles.button}>
            <Button
              shape="rectangle"
              justify="center"
              size="small"
              onClick={handleClose}
              className={Styles.cancelButton}
            >
              Cancel
            </Button>
            <Button
              shape="rectangle"
              color="primary"
              justify="center"
              size="small"
              type="submit"
            >
              Save
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default ProjectMasterDataEditForm;
