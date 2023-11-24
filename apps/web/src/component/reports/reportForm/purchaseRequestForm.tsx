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
import ReportService from '../../../service/report-service';
import PurcahseRegisterExcelReport from '../../reportGenerator/excelReport/purchaseRegister'

const PurchaseRequestForm: React.FC = (props: any) => {
  const purchaseControl: any = [
    { label: 'Local Purchase', value: 'Local Purchase' },
    { label: 'Head Office', value: 'Head Office' },
  ];
  const orderType: any = [{ label: 'Purchase Order', value: 'PO' }];
  const [initialValues, setInitialValues] = useState<any>({
    purchase_type: '',
    project_id: '',
    order_type: '',
    start_date: '',
    end_date: '',
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
      setTimeout(async () => {
        const obj: any = {
          order_by_column: "updated_date",
          order_by_direction: "desc",
          status: "AC",
          project_id: Number(values?.project_id),
          purchase_order_type: values?.purchase_type,
          from_order_date: values?.start_date,
          to_order_date: values?.end_date,
        }
        const reportsData = await ReportService.getPurchaseRegisterReport(obj)
        // if (reportsData?.total_count !== 0) {
          PurcahseRegisterExcelReport(reportsData?.content)
          props.setMessage('Report Generated Successfully');
        // }
        setLoader(false);
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
            <Select
              label="Purchase Type"
              name="purchase_type"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.purchase_type}
            >
              {purchaseControl?.map((item: any, index: any) => {
                return (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                );
              })}
            </Select>
          </div>
          <div>
            <AutoCompleteSelect
              name="project_id"
              label="Project Name"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.project_id}
              optionList={props.getAllProjectForDrop}
              onSelect={(value) => {
                formik.setFieldValue('project_id', value);
              }}
            />
          </div>
          {/* <div>
            <Select
              label="Order Type"
              name="order_type"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.order_type}
            >
              {orderType?.map((item: any, index: any) => {
                return (
                  <option key={item.value} value={item.value}>
                    {item.label}
                  </option>
                );
              })}
            </Select>
          </div> */}
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

export default PurchaseRequestForm;
