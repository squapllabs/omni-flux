export const vendorErrorMessages = {
  ENTER_VENDORNAME: 'Vendor name is required',
  ENTER_CONTACTPERSONNAME: 'Contact person name is required',
  ENTER_EMAIL: 'Contact email is required',
  ENTER_VALID_EMAIL: 'Please enter a valid email',
  ENTER_MOBILENUMBER: 'Mobile number is required',
  ENTER_VALID_MOBILENUMBER: 'Invalid mobile number',
  ENTER_VALID_NAME: 'Invalid name',
  ENTER_MAX_NAME: 'Name should not exceed 100 characters',
  ENTER_BANKACCOUNTNUMBER: 'Account number is required',
  ENTER_BANKIFSCCODE: 'IFSC code is required',
  ENTER_BANKHOLDERNAME: 'Account holder name is required',
  ENTER_PAYMENT: 'Select payment type',
  ENTER_CURRENCY: 'Select currency',
  ENTER_CATEGORY_ID: 'Select vendor category',
  ENTER_TAX: 'Tax Number is required',
};

export const getvendorcreationYupschema = (yup: any) => {
  return yup.object().shape({
    vendor_name: yup
      .string()
      .max(100, vendorErrorMessages.ENTER_MAX_NAME)
      .required(vendorErrorMessages.ENTER_VENDORNAME),
    contact_person: yup
      .string()
      .max(100, vendorErrorMessages.ENTER_MAX_NAME)
      .required(vendorErrorMessages.ENTER_CONTACTPERSONNAME),
    contact_email: yup
      .string()
      .required(vendorErrorMessages.ENTER_EMAIL)
      .email(vendorErrorMessages.ENTER_VALID_EMAIL),
    contact_phone_no: yup
      .string()
      .matches(/^\d{10}$/, vendorErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(vendorErrorMessages.ENTER_MOBILENUMBER),
    address: yup.object().shape({
      city: yup.string().matches(/^[A-Za-z]+$/, 'Invalid city name'),
      state: yup.string().matches(/^[A-Za-z\s]+$/, 'Invalid state name'),
      country: yup.string().matches(/^[A-Za-z]+$/, 'Invalid country name'),
      pin_code: yup.number().typeError('Only numbers are allowed'),
    }),
    bank_account_details: yup.object().shape({
      account_no: yup
        .number()
        .required(vendorErrorMessages.ENTER_BANKACCOUNTNUMBER)
        .typeError('Invalid Account number')
        .test(
          'is-valid-number',
          'Account number must have 8 to 16 digits',
          (value : any) => {
            const stringValue = String(value);
            return stringValue.length >= 8 && stringValue.length <= 16;
          }
        ),
      ifsc_code: yup
        .string()
        .required(vendorErrorMessages.ENTER_BANKIFSCCODE)
        .matches(/^[A-Z]{4}\d{7}$/, 'Invalid IFSC code'),
      acc_holder_name: yup
        .string()
        .required(vendorErrorMessages.ENTER_BANKHOLDERNAME),
    }),
    preferred_payment_method_id: yup
      .string()
      .required(vendorErrorMessages.ENTER_PAYMENT),
    currency: yup.string().required(vendorErrorMessages.ENTER_CURRENCY),
    vendor_category_id: yup
      .string()
      .required(vendorErrorMessages.ENTER_CATEGORY_ID),
    tax_id: yup.string().required(vendorErrorMessages.ENTER_TAX),
  });
};
