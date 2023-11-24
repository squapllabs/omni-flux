import React, { useState } from 'react';
// import Input from '../../ui/Input';
import Styles from '../../../styles/newStyles/reportModule/reportForm.module.scss';
// import Select from '../../ui/selectNew';
import DatePicker from '../../ui/CustomDatePicker';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import * as yup from 'yup';
import CustomLoader from '../../ui/customLoader';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import ReportService from '../../../service/report-service';
import RFQRegisterExcelReport from '../../reportGenerator/excelReport/requestForQuotationRegister'

const RFQRegisterForm: React.FC = (props: any) => {
  const [initialValues, setInitialValues] = useState<any>({
    project_name: '',
    order_type: '',
    end_date: '',
    start_date: '',
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
      setTimeout(async() => {
        const obj: any = {
          order_by_column: "updated_date",
          order_by_direction: "desc",
          status: "AC",
          project_id: Number(values?.project_id),
          purchase_order_type: values?.purchase_type,
          from_order_date: values?.start_date,
          to_order_date: values?.end_date,
        }
        const reportsData = await ReportService.getPurchaseRequestReport(obj)
        // if (reportsData?.total_count !== 0) {
          RFQRegisterExcelReport(reportsData?.content)
          // props.setMessage('Report Generated Successfully');
        // }
        // else {
          props.setMessage('No Records Found');
        // }
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
              label="Start Date"
              name="start_date"
              onChange={formik.handleChange}
              value={formik.values.start_date}
              error={
                formik.errors.start_date && formik.touched.start_date
                  ? true
                  : false
              }
              mandatory
            />
          </div>
          <div>
            <DatePicker
              label="End Date"
              name="end_date"
              onChange={formik.handleChange}
              value={formik.values.end_date}
              error={
                formik.errors.end_date && formik.touched.end_date ? true : false
              }
              mandatory
            />
          </div>
        </div>
        <div className={Styles.dividerLine}></div>
        <div className={Styles.finalButton}>
          <div>
            <Button
              type="button"
              color="cancel"
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
export default RFQRegisterForm;
