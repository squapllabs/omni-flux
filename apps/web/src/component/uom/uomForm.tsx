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
import Styles from '../../styles/newStyles/uomForm.module.scss';
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
      if (props.mode === 'EDIT') fetchOne();
    }
  }, [props.uomId, props.mode]);
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
            if (data?.message === 'success') {
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
            if (data?.message === 'success') {
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
  };

  return (
    <div>
      <div className={Styles.sub_container}>
        <div className={Styles.formFields}>
          <div style={{ width: '60%' }}>
            <div>
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
            <div>
              <TextArea
                name="description"
                label="Description"
                placeholder="Enter description"
                mandatory={true}
                value={formik.values.description}
                onChange={formik.handleChange}
                error={formik.touched.description && formik.errors.description}
                rows={5}
                maxCharacterCount={100}
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

export default UomForm;
