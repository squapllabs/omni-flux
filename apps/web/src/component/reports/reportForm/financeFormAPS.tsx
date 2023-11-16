import React, { useState } from 'react';
// import Input from '../../ui/Input';
import Styles from '../../../styles/newStyles/reportModule/reportForm.module.scss';
import Select from '../../ui/selectNew';
import DatePicker from '../../ui/CustomDatePicker';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import * as yup from 'yup';
import CustomLoader from '../../ui/customLoader';
import AutoCompleteSelect from '../../ui/AutoCompleteSelect';
import ReportService from '../../../service/report-service';
import POInvoiceItemExcelReport from '../../reportGenerator/excelReport/accountsProjectSummery'

const FinanceFormAPS: React.FC = (props: any) => {

  const paidStatus: any = [
    { label: 'Paid', value: 'Paid' },
    { label: 'Un Paid', value: 'To Be Paid' },
  ];

  const paymentType: any = [
    { label: 'Bank Transfer', value: 'Bank Transfer' },
    { label: 'Cheque', value: 'Cheque' },
  ];

  const [initialValues, setInitialValues] = useState<any>({
    project_name: '',
    start_date: '',
    end_date: '',
    paid_status: '',
    payment_type: '',
  });
  const [loader, setLoader] = useState(false);
    const validationSchema = yup.object().shape({
      start_date: yup.date().required(),
      end_date: yup.date().required(),
      paid_status:yup.string().required(),
    });
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      setLoader(true);
      setTimeout(async () => {
        const obj: any = {
          order_by_column: "updated_date",
          order_by_direction: "asc",
          status: values?.paid_status,
          project_id: Number(values?.project_id),
          payment_mode:values.payment_type,
          from_date: values?.start_date,
          to_date: values?.end_date,
        }
        // console.log("obkjr",obj);
        
        const reportsData = await ReportService.getPurchaseOrderInvoiceReport(obj)
        // if (reportsData?.total_count !== 0) {
          POInvoiceItemExcelReport(reportsData?.content)
          props.setMessage('Report Generated Successfully');
        // }
        // else {
        //   props.setMessage('No Records Found');
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
            <Select
              label="Payment Status"
              name="paid_status"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.paid_status}
              error={
                formik.errors.paid_status && formik.touched.paid_status
                  ? true
                  : false
              }
              mandatory
            >
              {paidStatus?.map((item: any, index: any) => {
                return (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                );
              })}
            </Select>
          </div>
          <div>
            <Select
              label="Payment Type"
              name="payment_type"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.payment_type}
            >
              {paymentType?.map((item: any, index: any) => {
                return (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                );
              })}
            </Select>
          </div>
          <div>
            <DatePicker
              label="From Date"
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
              label="To Date"
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

export default FinanceFormAPS;
