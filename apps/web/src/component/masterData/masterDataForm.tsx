import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import {
  useCreatemasterData,
  useGetAllParentmasertDataDrop,
  useUpdatemasterData,
} from '../../hooks/masertData-hook';
import {
  getCreateValidateyup,
  getUpdateValidateyup,
} from '../../helper/constants/master-constants';
import MasterService from '../../service/masterData-service';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import TextArea from '../ui/CustomTextArea';
import Styles from '../../styles/masterdata.module.scss';
import AutoCompleteSelect from '../ui/AutoCompleteSelect';

const MasterDataForm: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState({
    master_data_id: '',
    master_data_name: '',
    master_data_description: '',
    master_data_type: '',
    parent_master_data_id: '',
  });
  const [isButtonDisabled, setIsButtonDisabled] = useState(false);
  //   const [disable, setDisable] = useState(false);
  //   if (props.mode === 'Edit') {
  //     setDisable(true);
  //   }
  const { data: getAllmasterDataForDrop = [], isLoading: dropLoading } =
    useGetAllParentmasertDataDrop();
  const { mutate: postMasterData } = useCreatemasterData();
  const { mutate: updateMasterData } = useUpdatemasterData();
  const validationSchema =
    props.mode === 'Add'
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);

  useEffect(() => {
    const fetchOne = async () => {
      const data = await MasterService.getOnemasertDataByID(props.masterID);
      setInitialValues({
        master_data_id: data?.data?.master_data_id,
        master_data_name: data?.data?.master_data_name,
        master_data_description: data?.data?.master_data_description,
        master_data_type: data?.data?.master_data_type,
        parent_master_data_id: data?.data?.parent_master_data_id,
      });
    };
    if (props.mode === 'Edit') fetchOne();
  }, [props.masterID, props.mode]);

  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      setIsButtonDisabled(true);
      setTimeout(() => {
        setIsButtonDisabled(false);
      }, 2000);
      if (props.mode === 'Add') {
        let object: any = {};
        const num = 0;
        if (Number(values.parent_master_data_id) === num) {
          object = {
            master_data_name: values.master_data_name,
            master_data_description: values.master_data_description,
            master_data_type: values.master_data_type,
            parent_master_data_id: null,
          };
        } else {
          object = {
            master_data_name: values.master_data_name,
            master_data_description: values.master_data_description,
            master_data_type: values.master_data_type,
            parent_master_data_id: Number(values.parent_master_data_id),
          };
        }
        postMasterData(object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              props.setMessage('Master Data created');
              props.setOpenSnack(true);
              props.setOpen(false);
              resetForm();
            }
          },
        });
      } else {
        const Object: any = {
          master_data_id: values.master_data_id,
          master_data_name: values.master_data_name,
          master_data_description: values.master_data_description,
          master_data_type: values.master_data_type,
          parent_master_data_id: Number(values.parent_master_data_id),
        };
        updateMasterData(Object, {
          onSuccess: (data, variables, context) => {
            if (data?.message === 'success') {
              resetForm();
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Master Data edited');
              props.setOpenSnack(true);
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
      <div className={Styles.sub_container}>
        <div className={Styles.formFields}>
          <div style={{ width: '60%' }}>
            <div>
              <Input
                name="master_data_name"
                label="Name"
                placeholder="Enter master name"
                value={formik.values.master_data_name}
                onChange={formik.handleChange}
                mandatory={true}
                error={
                  formik.touched.master_data_name &&
                  formik.errors.master_data_name
                }
                width="300px"
              />
            </div>
            <div>
              <Input
                name="master_data_type"
                label="Code"
                placeholder="Enter code"
                value={formik.values.master_data_type}
                onChange={formik.handleChange}
                mandatory={true}
                error={
                  formik.touched.master_data_type &&
                  formik.errors.master_data_type
                }
                disabled={props.mode === 'Add' ? false : true}
                width="300px"
              />
            </div>
            <div>
              <AutoCompleteSelect
                label="Parent Name"
                name="parent_master_data_id"
                onChange={formik.handleChange}
                value={formik.values.parent_master_data_id}
                placeholder="Select from options"
                width="300px"
                onSelect={(value) => {
                  formik.setFieldValue('parent_master_data_id', value);
                }}
                optionList={dropLoading === true ? [] : getAllmasterDataForDrop}
                error={
                  formik.touched.parent_master_data_id &&
                  formik.errors.parent_master_data_id
                }
                disabled={props.mode === 'Add' ? false : true}
              />
            </div>
            <div>
              <TextArea
                name="master_data_description"
                label="Description"
                placeholder="Enter description"
                value={formik.values.master_data_description}
                onChange={formik.handleChange}
                mandatory={true}
                error={
                  formik.touched.master_data_description &&
                  formik.errors.master_data_description
                }
                rows={5}
                maxCharacterCount={120}
                width="300px"
              />
            </div>
          </div>
        </div>
        <div className={Styles.bottom_container}>
          <div className={Styles.footer1}>
            <div>
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
                  onClick={formik.handleSubmit}
                  disabled={isButtonDisabled}
                >
                  Save
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default MasterDataForm;
