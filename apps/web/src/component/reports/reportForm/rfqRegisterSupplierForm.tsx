import React, { useState } from 'react';
import Input from '../../ui/Input';
import Styles from '../../../styles/newStyles/reportModule/reportForm.module.scss';
import Select from '../../ui/selectNew';
import DatePicker from '../../ui/CustomDatePicker';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import * as yup from 'yup';
import CustomLoader from '../../ui/customLoader';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';

const RFQRegisterSupplierForm: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState<any>({
    project_name: '',
    end_date: '',
    start_date: '',
    vendor_name: '',
  });
  const [loader, setLoader] = useState(false);
  const validationSchema = yup.object().shape({
    start_date: yup.date().required(),
    end_date: yup.date().required(),
  });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      setLoader(true);
      setTimeout(() => {
        const url =
          'https://zpaisa-purchase-sale-docs.s3.ap-south-1.amazonaws.com/OmniFlux/PR300/file-1699171364952-390447409-RFQ-Register(Vendor) (1).xlsx';
        const link = document.createElement('a');
        link.href = url;
        link.click();
        setLoader(false);
        props.setMessage('Report Generated Successfully');
        props.setOpenSnack(true);
        props.setOpen(false);
      }, 1000);
    },
  });
  return (
    <div>
      <CustomLoader loading={loader}>
        <div className={Styles?.container}>
          <div>
            <AutoCompleteSelect
              name="project_name"
              label="Project Name"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.project_name}
              optionList={props.getAllProjectForDrop}
              onSelect={(value) => {
                formik.setFieldValue('project_name', value);
              }}
            />
          </div>
          <div>
            <Input
              name="vendor_name"
              label="Vendor Name"
              onChange={formik.handleChange}
              value={formik.values.vendor_name}
            />
          </div>

          <div>
            <DatePicker
              label="RFQ Start Date"
              name="start_date"
              onChange={formik.handleChange}
              value={formik.values.start_date}
              error={
                formik.errors.start_date && formik.touched.start_date
                  ? true
                  : false
              }
            />
          </div>
          <div>
            <DatePicker
              label="RFQ End Date"
              name="end_date"
              onChange={formik.handleChange}
              value={formik.values.end_date}
              error={
                formik.errors.end_date && formik.touched.end_date ? true : false
              }
            />
          </div>
        </div>
        <div className={Styles.dividerLine}></div>
        <div className={Styles.finalButton}>
          <div>
            <Button
              type="button"
              color="secondary"
              shape="rectangle"
              size="small"
              justify="center"
              onClick={() => {
                props.setOpen(false);
              }}
            >
              Cancel
            </Button>
          </div>
          <div>
            <Button
              type="submit"
              color="primary"
              shape="rectangle"
              size="small"
              justify="center"
              onClick={formik.handleSubmit}
            >
              Generate Report
            </Button>
          </div>
        </div>
      </CustomLoader>
    </div>
  );
};

export default RFQRegisterSupplierForm;
