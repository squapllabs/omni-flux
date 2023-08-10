export const leadProductErrorMessages = {
  SELECT_CLIENT: 'Client is required',
  SELECT_CLIENT_LEVEL: 'Client Level is required',
  SELECT_LEAD_SOURCE: 'Lead source is required',
  SELECT_LEAD_PROBABILITY: 'Probability is required',
  ENTER_CLIENT_NAME: 'Client contact name is required',
  ENTER_APPROX_VALUE: 'Approx value is required',
  SELECT_SALES_PERSON: 'Sales person is required',
  CLIENT_REMARKS: 'Client remarks is required',
  OUR_REMARKS: 'Our remarks is required',
  MAXIMUM_LETTER: '200 characters allowed',
  ENTER_QUANTITY: 'Quantity is Required',
  ENTER_PRODUCT: 'Product is Required',
  MINIMUM_CHECK: 'Value must be greater than 0',
  MAXIMUM_CHECK: 'Value must be less then 100000',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    client: yup
      .string()
      .typeError(leadProductErrorMessages.SELECT_CLIENT)
      .required(leadProductErrorMessages.SELECT_CLIENT),
    client_level: yup
      .string()
      .typeError(leadProductErrorMessages.SELECT_CLIENT_LEVEL)
      .required(leadProductErrorMessages.SELECT_CLIENT_LEVEL),
    source_name: yup
      .string()
      .typeError(leadProductErrorMessages.SELECT_LEAD_SOURCE)
      .required(leadProductErrorMessages.SELECT_LEAD_SOURCE),
    probability: yup
      .string()
      .typeError(leadProductErrorMessages.SELECT_LEAD_PROBABILITY)
      .required(leadProductErrorMessages.SELECT_LEAD_PROBABILITY),
    client_contact_name: yup
      .string()
      .typeError(leadProductErrorMessages.ENTER_CLIENT_NAME)
      .required(leadProductErrorMessages.ENTER_CLIENT_NAME),
    approx_value: yup
      .number()
      .min(1, leadProductErrorMessages.MINIMUM_CHECK)
      .max(100000, leadProductErrorMessages.MAXIMUM_CHECK)
      .typeError(leadProductErrorMessages.ENTER_APPROX_VALUE)
      .required(leadProductErrorMessages.ENTER_APPROX_VALUE),
    sales_person_name: yup
      .string()
      .typeError(leadProductErrorMessages.SELECT_SALES_PERSON)
      .required(leadProductErrorMessages.SELECT_SALES_PERSON),
    // client_remark: yup
    //   .string()
    //   .typeError(leadProductErrorMessages.CLIENT_REMARKS)
    //   .required(leadProductErrorMessages.CLIENT_REMARKS)
    //   .max(200, leadProductErrorMessages.MAXIMUM_LETTER),
    // our_remarks: yup
    //   .string()
    //   .typeError(leadProductErrorMessages.OUR_REMARKS)
    //   .required(leadProductErrorMessages.OUR_REMARKS)
    //   .max(200, leadProductErrorMessages.MAXIMUM_LETTER),
  });
};
export const getUpdateValidateyup = (yup: any) => {
  return yup.object().shape({});
};
export const getValidateProductyup = (yup: any) => {
  return yup.object().shape({
    product_id: yup
      .string()
      .typeError(leadProductErrorMessages.ENTER_PRODUCT)
      .required(leadProductErrorMessages.ENTER_PRODUCT),
    quantity: yup
      .string()
      .typeError(leadProductErrorMessages.ENTER_QUANTITY)
      .required(leadProductErrorMessages.ENTER_QUANTITY),
  });
};
