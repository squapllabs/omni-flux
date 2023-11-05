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

const FinanceFormAMS: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState<any>({
    project_name: '',
    year: '',
  });
  const [loader, setLoader] = useState(false);
  //   const validationSchema = yup.object().shape({
  //     start_date: yup.date().required(),
  //     end_date: yup.date().required(),
  //   });
  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      setLoader(true);
      setTimeout(() => {
        const url =
          'https://zpaisa-purchase-sale-docs.s3.ap-south-1.amazonaws.com/OmniFlux/PR300/file-1699181431175-385365854-FA-Month Summery.xlsx';
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
            <DatePicker
              name="year"
              label="Year"
              onChange={formik.handleChange}
              value={formik.values.year}
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
export default FinanceFormAMS;
