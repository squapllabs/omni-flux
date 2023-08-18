import React, { useState, useEffect } from 'react';
import { useFormik } from 'formik';
import { createuom, updateUom } from '../../hooks/uom-hooks';
import {
  getuomCreateValidateyup,
  getuomUpdateValidateyup,
} from '../../helper/constants/uom-constants';
import uomService from '../../service/uom-service';
import * as Yup from 'yup';
import Input from '../../component/ui/Input';
import Button from '../ui/Button';
import Styles from '../../styles/userList.module.scss';
import CancelIcon from '../menu/icons/closeIcon'
import TextArea from '../ui/CustomTextArea';

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

  const handleClose = () => {
    props.setOpen(false);
  }

  return (
    <div className={Styles.formContainer}>
      <form onSubmit={formik.handleSubmit}>
        <div className={Styles.header}>
          <div><h4 className={Styles.titleStyle}>Edit UOM</h4></div>
          <div> <CancelIcon onClick={handleClose} /></div>
        </div>
        <div className={Styles.dividerStyle}></div>
        <div className={Styles.field}>
          <Input
            name="name"
            label="Unit Of Measurement"
            placeholder="Enter unit of measurement"
            mandatory={true}
            value={formik.values.name}
            onChange={formik.handleChange}
            error={formik.touched.name && formik.errors.name}
          />
        </div>
        <div className={Styles.field}>
          <TextArea
            name="description"
            label="Description"
            placeholder="Enter description"
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
    </div >
  );
};

export default UomForm;
