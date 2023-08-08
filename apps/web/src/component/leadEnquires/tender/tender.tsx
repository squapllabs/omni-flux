import React, { useEffect, useState } from 'react';
import Styles from '../../../styles/leadTender.module.scss';
import Input from '../../ui/Input';
import Select from '../../ui/selectNew';
import DatePicker from '../../ui/CustomDatePicker';
import Button from '../../ui/Button';
import AddIcon from '../../menu/icons/addIcon';
import { useFormik } from 'formik';
import {
  useGetAllleadEnquiry,
  createleadEnquiry,
  updateleadEnquiry,
} from 'apps/web/src/hooks/leadEnquires-hooks';
import { useGetAllClient } from 'apps/web/src/hooks/client-hooks';
import { getBymasertDataType } from 'apps/web/src/hooks/masertData-hook';
import {
  getCreateValidateyup,
  getUpdateValidateyup,
} from 'apps/web/src/helper/constants/lead/leadTender-constant';
import * as Yup from 'yup';
import LeadEnquiresServices from 'apps/web/src/service/leadEnquires-services';
import { format } from 'date-fns';
import { useNavigate } from 'react-router-dom';
import CustomSnackBar from '../../ui/customSnackBar';
import { formatBudgetValue } from '../../../helper/common-function';
import { environment } from '../../../environment/environment';

const Tender: React.FC = (props: any) => {
  const navigate = useNavigate();
  const validationSchema =
    props.leadEnquireId === undefined
      ? getCreateValidateyup(Yup)
      : getUpdateValidateyup(Yup);
  const [initialValues, setInitialValues] = useState({
    lead_enquiry_id: '',
    lead_type: props.leadType,
    client: '',
    client_level: '',
    client_contact_name: '',
    client_contact_email: '',
    client_contact_phone: '',
    our_remarks: '',
    client_remark: '',
    doc_url: '',
    status_remarks: '',
    source_name: '',
    status: '',
    probability: '',
    approx_value: '',
    sales_person_name: '',
    created_by: '',
    tender_reg_no: '',
    tender_identification_no: '',
    tender_name: '',
    tender_issue_date: '',
    tender_due_date: '',
    tender_type: '',
    estimated_value: '',
    industry_sector: '',
    lead_tender_id: '',
    lead_code: '',
  });
  const [disable, setDisable] = useState(
    props?.leadEnquireId != undefined ? true : false
  );
  const [selectedValue, setSelectedValue] = useState('');
  const [openSnack, setOpenSnack] = useState(false);
  const [message, setMessage] = useState('');
  const [appendedValue, setAppendedValue] = useState('');
  const { mutate: postleadEnquiry } = createleadEnquiry();
  const { mutate: updatelead } = updateleadEnquiry();
  const { data: getAllClient = [] } = useGetAllClient();
  const { data: getClientLevel = [] } = getBymasertDataType('CTLVL');
  const { data: getAllIndustrySector = [] } = getBymasertDataType('INSEC');
  const { data: getAllTenderType = [] } = getBymasertDataType('TDTE');
  const handleSnackBarClose = () => {
    setOpenSnack(false);
  };
  const dateFormat = (value: any) => {
    const currentDate = new Date(value);
    let formattedDate = format(currentDate, 'yyyy-MM-dd');
    return formattedDate;
  };

  const handleValueChange = (event: any) => {
    const budgetValue = event.target.value;
    const data = formatBudgetValue(Number(budgetValue));
    setAppendedValue(data);
    formik.setFieldValue('estimated_value', budgetValue);
    formik.handleChange(event);
  };

  const inputLabelNameFromEnv = `Estimate Value (${environment.INPUTBUDGET})`;
  const outputLableNameFromEnv = `Estimate Value (${environment.OUTPUTBUDGET})`;
  useEffect(() => {
    const fetchData = async () => {
      let data = await LeadEnquiresServices.getOneleadEnquiryByID(
        props.leadEnquireId
      );
      console.log('datagetOne', data);
      setInitialValues({
        lead_enquiry_id: props.leadEnquireId,
        lead_type: props.leadType,
        client: data?.data?.client,
        client_level: data?.data?.client_level,
        client_contact_name: data?.data?.client_contact_name,
        client_contact_email: data?.data?.client_contact_email,
        client_contact_phone: data?.data?.client_contact_phone,
        our_remarks: '',
        client_remark: '',
        doc_url: '',
        status_remarks: '',
        source_name: '',
        status: '',
        probability: '',
        approx_value: '',
        sales_person_name: '',
        created_by: '',
        tender_reg_no: data?.data?.lead_enquiry_tenders[0]?.tender_reg_no,
        tender_identification_no:
          data?.data?.lead_enquiry_tenders[0]?.tender_identification_no,
        tender_name: data?.data?.lead_enquiry_tenders[0]?.tender_name,
        tender_issue_date: dateFormat(
          data?.data?.lead_enquiry_tenders[0]?.tender_issue_date
        ),
        tender_due_date: dateFormat(
          data?.data?.lead_enquiry_tenders[0]?.tender_due_date
        ),
        tender_type: data?.data?.lead_enquiry_tenders[0]?.tender_type,
        estimated_value: data?.data?.lead_enquiry_tenders[0]?.estimated_value,
        industry_sector: data?.data?.lead_enquiry_tenders[0]?.industry_sector,
        lead_tender_id: data?.data?.lead_enquiry_tenders[0]?.lead_tender_id,
        lead_code: data?.data?.lead_code,
      });
    };
    fetchData();
  }, [props.leadEnquireId]);
  const formik = useFormik({
    initialValues,
    validationSchema,
    enableReinitialize: true,
    onSubmit: (values, { resetForm }) => {
      if (values) {
        if (props.leadEnquireId === undefined) {
          let object: any = {
            lead_type: props.leadType,
            client: Number(values.client),
            client_level: Number(values.client_level),
            client_contact_name: values.client_contact_name,
            client_contact_email: values.client_contact_email,
            client_contact_phone: values.client_contact_phone,
            our_remarks: values.our_remarks,
            client_remark: values.client_remark,
            doc_url: '',
            status_remarks: '',
            source_name: values.source_name,
            status: '',
            probability: Number(values.probability),
            approx_value: Number(values.approx_value),
            sales_person_name: Number(values.sales_person_name),
            created_by: 56,
            tender_reg_no: values.tender_reg_no,
            tender_identification_no: values.tender_identification_no,
            tender_name: values.tender_name,
            tender_issue_date: values.tender_issue_date,
            tender_due_date: values.tender_due_date,
            tender_type: values.tender_type,
            estimated_value: Number(values.estimated_value),
            industry_sector: Number(values.industry_sector),
          };
          postleadEnquiry(object, {
            onSuccess(data, variables, context) {
              resetForm;
              setMessage('lead Tender has created successfully');
              setOpenSnack(true);
              setInterval(() => {
                navigate('/lead-enquires');
              }, 3000);
            },
          });
        } else {
          let object: any = {
            lead_type: props.leadType,
            client: Number(values.client),
            client_level: Number(values.client_level),
            client_contact_name: values.client_contact_name,
            client_contact_email: values.client_contact_email,
            client_contact_phone: values.client_contact_phone,
            our_remarks: values.our_remarks,
            client_remark: values.client_remark,
            doc_url: '',
            status_remarks: '',
            source_name: values.source_name,
            status: '',
            probability: Number(values.probability),
            approx_value: Number(values.approx_value),
            sales_person_name: Number(values.sales_person_name),
            created_by: 56,
            tender_reg_no: values.tender_reg_no,
            tender_identification_no: values.tender_identification_no,
            tender_name: values.tender_name,
            tender_issue_date: values.tender_issue_date,
            tender_due_date: values.tender_due_date,
            tender_type: values.tender_type,
            estimated_value: Number(values.estimated_value),
            industry_sector: Number(values.industry_sector),
            lead_tender_id: Number(values.lead_tender_id),
            lead_enquiry_id: Number(props.leadEnquireId),
          };
          updatelead(object, {
            onSuccess(data, variables, context) {
              console.log(data);
              setMessage('lead Tender has updated successfully');
              setOpenSnack(true);
              setInterval(() => {
                navigate('/lead-enquires');
              }, 3000);
            },
          });
        }
      }
    },
  });
  return (
    <div>
      <div className={Styles.box}>
        <form onSubmit={formik.handleSubmit}>
          <div className={Styles.fields_container}>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Lead #"
                  name="lead_code"
                  value={formik.values.lead_code}
                  onChange={formik.handleChange}
                  disabled={disable}
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Tender Registration No"
                  name="tender_reg_no"
                  value={formik.values.tender_reg_no}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.tender_reg_no && formik.errors.tender_reg_no
                  }
                  disabled={disable}
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Tender Identification #"
                  name="tender_identification_no"
                  value={formik.values.tender_identification_no}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.tender_identification_no &&
                    formik.errors.tender_identification_no
                  }
                  disabled={disable}
                />
              </div>
              <div className={Styles.fieldStyle}>
                <DatePicker
                  label="Target Issued Date"
                  name="tender_issue_date"
                  value={formik.values.tender_issue_date}
                  onChange={formik.handleChange}
                  InputProps={{
                    inputProps: {
                      min: '1930-01-01',
                      max: `${new Date().toISOString().slice(0, 10)}`,
                    },
                  }}
                  error={
                    formik.touched.tender_issue_date &&
                    formik.errors.tender_issue_date
                  }
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Tender Name"
                  name="tender_name"
                  value={formik.values.tender_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.tender_name && formik.errors.tender_name
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <DatePicker
                  label="Target Due Date"
                  name="tender_due_date"
                  value={formik.values.tender_due_date}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.tender_due_date &&
                    formik.errors.tender_due_date
                  }
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Select
                  name="tender_type"
                  label="Tender Type"
                  defaultLabel="Select a Tender"
                  value={formik.values.tender_type}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.tender_type && formik.errors.tender_type
                  }
                >
                  {getAllTenderType?.map((option: any) => (
                    <option
                      key={option.master_data_name}
                      value={option.master_data_name}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div className={Styles.fieldStyle}>
                <div style={{ display: 'flex', gap: '10px' }}>
                  <Input
                    label="Estimate Value"
                    name="estimated_value"
                    value={formik.values.estimated_value}
                    onChange={handleValueChange}
                    error={
                      formik.touched.estimated_value &&
                      formik.errors.estimated_value
                    }
                  />{' '}
                  <Input
                    name="label_field"
                    label={outputLableNameFromEnv}
                    value={appendedValue}
                  />
                </div>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Select
                  name="industry_sector"
                  label="Industry/Sector"
                  defaultLabel="select a Industry sector"
                  value={formik.values.industry_sector}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.industry_sector &&
                    formik.errors.industry_sector
                  }
                >
                  {getAllIndustrySector?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_id}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
              <div className={Styles.fieldStyle}></div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Select
                  name="client"
                  label="Client"
                  defaultLabel="select Client"
                  value={formik.values.client}
                  onChange={formik.handleChange}
                  error={formik.touched.client && formik.errors.client}
                >
                  {getAllClient?.map((option: any) => (
                    <option key={option.client_id} value={option.client_id}>
                      {option.name}
                    </option>
                  ))}
                </Select>
              </div>
              <div className={Styles.fieldStyle}>
                <Select
                  name="client_level"
                  label="Client Level"
                  defaultLabel="select Client"
                  value={formik.values.client_level}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.client_level && formik.errors.client_level
                  }
                >
                  {getClientLevel?.map((option: any) => (
                    <option
                      key={option.master_data_id}
                      value={option.master_data_id}
                    >
                      {option.master_data_name}
                    </option>
                  ))}
                </Select>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Client Contact Name"
                  name="client_contact_name"
                  value={formik.values.client_contact_name}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.client_contact_name &&
                    formik.errors.client_contact_name
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Client Contact Email"
                  name="client_contact_email"
                  value={formik.values.client_contact_email}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.client_contact_email &&
                    formik.errors.client_contact_email
                  }
                />
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}>
                <Input
                  label="Client Contact Phone"
                  name="client_contact_phone"
                  value={formik.values.client_contact_phone}
                  onChange={formik.handleChange}
                  error={
                    formik.touched.client_contact_phone &&
                    formik.errors.client_contact_phone
                  }
                />
              </div>
              <div className={Styles.fieldStyle}>
                <div className={Styles.button_container}>
                  <Button
                    shape="rectangle"
                    justify="center"
                    size="small"
                    color="primary"
                  >
                    Submit
                  </Button>
                </div>
              </div>
            </div>
            <div className={Styles.fields_container_1}>
              <div className={Styles.fieldStyle}></div>
              <div className={Styles.fieldStyle}>
                {/* <div className={Styles.button_container}>
                  <Button
                    shape="rectangle"
                    justify="center"
                    size="small"
                    color="primary"
                    icon={<AddIcon />}
                  >
                    Add Tender
                  </Button>
                </div> */}
              </div>
            </div>
          </div>
        </form>
      </div>
      {/* <div className={Styles.box}></div> */}
      <CustomSnackBar
        open={openSnack}
        message={message}
        onClose={handleSnackBarClose}
        autoHideDuration={1000}
        type="success"
      />
    </div>
  );
};

export default Tender;
