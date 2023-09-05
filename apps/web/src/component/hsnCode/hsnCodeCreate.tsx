import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { createHsnCode, updateHsnCode } from '../../hooks/hsnCode-hooks';
import {
  gethsnCreateValidateyup,
  gethsnUpdateValidateyup,
} from '../../helper/constants/hsn-constants';
import HsnCodeService from '../../service/hsnCode-service';
import * as Yup from 'yup';
import Input from '../ui/Input';
import Button from '../ui/Button';
import CancelIcon from '../menu/icons/closeIcon';
import Styles from '../../styles/gstList.module.scss';
import TextArea from '../ui/CustomTextArea';

const HsnCodeForm: React.FC = (props: any, { mode, id }) => {
  const [initialValues, setInitialValues] = useState({
    hsn_code_id: '',
    code: '',
    description: '',
  });

  useEffect(() => {
    if (props.mode === 'EDIT') {
      const fetchOne = async () => {
        const data = await HsnCodeService.getOneHsnCode(props.hsnCodeId);
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
            if (data?.mesage === 'success') {
              props.setOpen(false);
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
            if (data?.mesage === 'success') {
              props.setOpen(false);
              props.setReload(true);
              props.setMessage('Hsn Code edited');
              props.setOpenSnack(true);
            }
          },
        });
      }
    },
  });

  const handleClose = () => {
    props.setOpen(false);
  }

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.header}>
          <div><h4 className={Styles.titleStyle}>Edit HSN Code</h4></div>
          <div> <CancelIcon onClick={handleClose} /></div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Input
            label="Code"
            placeholder="Enter product code"
            name="code"
            mandatory={true}
            value={formik.values.code}
            onChange={formik.handleChange}
            error={formik.touched.code && formik.errors.code}
            width="100%"
          />
        </div>
        <div className={Styles.field}>
          <TextArea
            label="Description"
            placeholder="Enter product description"
            name="description"
            mandatory={true}
            value={formik.values.description}
            onChange={formik.handleChange}
            error={formik.touched.description && formik.errors.description}
            rows={3}
            maxCharacterCount={100}
          />
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.formButton}>
          <div>
            <Button className={Styles.cancelButton} shape="rectangle" justify="center" size="small" onClick={handleClose}>
              Cancel
            </Button>
          </div>
          <div>
            <Button color="primary" shape="rectangle" justify="center" size="small">
              Submit
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
};

export default HsnCodeForm;
