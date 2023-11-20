import LeadEnquiresServices from '../../../service/leadEnquires-services';

export const leadProductErrorMessages = {
  SELECT_CLIENT: 'Client is required',
  SELECT_CLIENT_LEVEL: 'Client Level is required',
  SELECT_LEAD_SOURCE: 'Lead source is required',
  SELECT_LEAD_PROBABILITY: 'Probability is required',
  ENTER_CLIENT_NAME: 'Client contact name is required',
  ENTER_CLIENT_EMAIL: 'Email is required',
  ENTER_CLIENT_PHONE: 'Phone number is required',
  ENTER_TENDER_REQ: 'Tender registration is required',
  ENTER_TENDER_INDETIFICATION: 'Tender identification is required',
  SELECT_TENDER_ISSUE_DATE: 'Tender issue date is required',
  SELECT_TENDER_DUE_DATE: 'Tender due date is required',
  SELECT_TENDER_TYPE: 'Tender Type is required',
  ENTER_ESTIMATE_VALUE: 'Estimate value is required',
  SELECT_INDUSTRY_SECTOR: 'Industry sector is required',
  ENTER_TENDER_NAME: 'Tender name is required',
  MINIMUM_CHECK: 'Value must be greater than 0',
  MAXIMUM_CHECK: 'Value must be less then 100000',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    client: yup
      .string()
      .trim()
      .typeError(leadProductErrorMessages.SELECT_CLIENT)
      .required(leadProductErrorMessages.SELECT_CLIENT),
    client_level: yup
      .string()
      .typeError(leadProductErrorMessages.SELECT_CLIENT_LEVEL)
      .required(leadProductErrorMessages.SELECT_CLIENT_LEVEL),
    client_contact_name: yup
      .string()
      .typeError(leadProductErrorMessages.ENTER_CLIENT_NAME)
      .required(leadProductErrorMessages.ENTER_CLIENT_NAME),
    client_contact_email: yup
      .string()
      .email('Invalid email address')
      .typeError(leadProductErrorMessages.ENTER_CLIENT_EMAIL)
      .required(leadProductErrorMessages.ENTER_CLIENT_EMAIL),
    client_contact_phone: yup
      .string()
      .matches(/^\+?[0-9]+$/, 'Invalid phone number')
      .typeError(leadProductErrorMessages.ENTER_CLIENT_PHONE)
      .required(leadProductErrorMessages.ENTER_CLIENT_PHONE),
    tender_reg_no: yup
      .string()
      .trim()
      .test('code-avilability', 'code is already exist', async (value: any) => {
        if (value) {
          const response = await LeadEnquiresServices.checkDublicateTenderReg(
            value
          );
          console.log('response', response);

          if (response?.status === true) {
            return false;
          } else {
            return true;
          }
        }
      })
      .typeError(leadProductErrorMessages.ENTER_TENDER_REQ)
      .required(leadProductErrorMessages.ENTER_TENDER_REQ),
    tender_identification_no: yup
      .string()
      .trim()
      .test('code-avilability', 'code is already exist', async (value: any) => {
        if (value) {
          const response =
            await LeadEnquiresServices.checkDublicateTenderIdentiNo(value);
          console.log('response', response);

          if (response?.status === true) {
            return false;
          } else {
            return true;
          }
        }
      })
      .typeError(leadProductErrorMessages.ENTER_TENDER_INDETIFICATION)
      .required(leadProductErrorMessages.ENTER_TENDER_INDETIFICATION),
    tender_name: yup
      .string()
      .trim()
      .typeError(leadProductErrorMessages.ENTER_TENDER_NAME)
      .required(leadProductErrorMessages.ENTER_TENDER_NAME),
    tender_issue_date: yup
      .date()
      .typeError(leadProductErrorMessages.SELECT_TENDER_ISSUE_DATE)
      .required(leadProductErrorMessages.SELECT_TENDER_ISSUE_DATE),
    tender_due_date: yup
      .date()
      .min(
        yup.ref('tender_issue_date'),
        'End date cannot be earlier than start date'
      )
      .test(
        'is-greater',
        'End date must be greater than the start date',
        function (value: string | number | Date, { parent }: yup.TestContext) {
          const startDate = parent.tender_issue_date;
          if (!startDate || !value) return true;
          return new Date(value) > new Date(startDate);
        }
      )
      .typeError(leadProductErrorMessages.SELECT_TENDER_DUE_DATE)
      .required(leadProductErrorMessages.SELECT_TENDER_DUE_DATE),
    tender_type: yup
      .string()
      .trim()
      .typeError(leadProductErrorMessages.SELECT_TENDER_TYPE)
      .required(leadProductErrorMessages.SELECT_TENDER_TYPE),
    estimated_value: yup
      .number()
      .min(1, leadProductErrorMessages.MINIMUM_CHECK)
      .max(100000, leadProductErrorMessages.MAXIMUM_CHECK)
      .typeError(leadProductErrorMessages.ENTER_ESTIMATE_VALUE)
      .required(leadProductErrorMessages.ENTER_ESTIMATE_VALUE),
    industry_sector: yup
      .string()
      .trim()
      .typeError(leadProductErrorMessages.SELECT_INDUSTRY_SECTOR)
      .required(leadProductErrorMessages.SELECT_INDUSTRY_SECTOR),
  });
};
export const getUpdateValidateyup = (yup: any) => {
  return yup.object().shape({});
};
