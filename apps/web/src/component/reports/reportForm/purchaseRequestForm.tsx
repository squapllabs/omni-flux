import React, { useState } from 'react';
import Input from '../../ui/Input';
import Styles from '../../../styles/newStyles/reportModule/reportForm.module.scss';
import Select from '../../ui/selectNew';
import DatePicker from '../../ui/CustomDatePicker';
import Button from '../../ui/Button';
import { useFormik } from 'formik';
import * as yup from 'yup';
import CustomLoader from '../../ui/customLoader';

const PurchaseRequestForm: React.FC = (props: any) => {
  const purchaseControl: any = [
    { label: 'Local Purchase', value: 'LP' },
    { label: 'Head Office', value: 'HO' },
  ];
  const orderType: any = [{ label: 'Purchase Order', value: 'PO' }];
  const [initialValues, setInitialValues] = useState<any>({
    purchase_control: '',
    project_name: '',
    order_type: '',
    order_date: '',
    release_date: '',
  });
  const [loader, setLoader] = useState(false);
  const formik = useFormik({
    initialValues,
    // validationSchema,
    enableReinitialize: true,
    onSubmit: async (values) => {
      setLoader(true);
      setTimeout(() => {
        const url =
          'https://zpaisa-purchase-sale-docs.s3.ap-south-1.amazonaws.com/OmniFlux/PR238/file-1699163324335-552782159-PO-Register%20(1).xlsx';
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
          <div style={{ width: '100%' }}>
            <Select
              label="Purchase Control"
              name="purchase_control"
              defaultLabel="Select Option"
              placeholder="Select Option"
              onChange={formik.handleChange}
              value={formik.values.purchase_control}
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
          <div style={{ width: '100%' }}>
            <Input
              name="project_name"
              label="Project Name"
              onChange={formik.handleChange}
              value={formik.values.project_name}
            />
          </div>
          <div style={{ width: '100%' }}>
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
          </div>
          <div>
            <DatePicker
              label="Order Date"
              name="order_date"
              onChange={formik.handleChange}
              value={formik.values.release_date}
            />
          </div>
          <div>
            <DatePicker
              label="Release Date"
              name="release_date"
              onChange={formik.handleChange}
              value={formik.values.release_date}
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

export default PurchaseRequestForm;
