import vendorService from '../../service/vendor-service';

export const vendorErrorMessages = {
  ENTER_VENDORNAME: 'Vendor name is required',
  ENTER_CODE:'Vendor Code is required',
  VENDOR_EXISTS: 'Vendor already exists',
  VENDOR_CODE_EXISTS: 'Vendor Code already exists',
  MIN_CODE: 'Code must be more then 4',
  MAX_CODE: 'Code must lesser then 10',
  ENTER_CONTACTPERSONNAME: 'Contact person name is required',
  ENTER_EMAIL: 'Contact email is required',
  ENTER_VALID_EMAIL: 'Please enter a valid email',
  ENTER_MOBILENUMBER: 'Mobile number is required',
  ENTER_VALID_MOBILENUMBER: 'Invalid mobile number',
  ENTER_VALID_NAME: 'Invalid name',
  ENTER_ONLY_TEXT: 'Only characters are allowed',
  ENTER_MAX_NAME: 'Name should not exceed 100 characters',
  ENTER_BANKACCOUNTNUMBER: 'Account number is required',
  ENTER_BANKIFSCCODE: 'IFSC code is required',
  ENTER_BANKHOLDERNAME: 'Account holder name is required',
  ENTER_PAYMENT: 'Select payment type',
  ENTER_CURRENCY: 'Select currency',
  ENTER_CATEGORY_ID: 'Select vendor category',
  ENTER_TAX: 'Tax Number is required',
  EMAIL_EXISTS: 'Email ID already exists',
};

export const getVendorCreationYupschema = (yup: any) => {
  return yup.object().shape({
    vendor_name: yup
      .string()
      .max(100, vendorErrorMessages.ENTER_MAX_NAME)
      .required(vendorErrorMessages.ENTER_VENDORNAME)
      .test(
        'vendor-name dublicate',
        vendorErrorMessages.VENDOR_EXISTS,
        async (value: any) => {
          if (value) {
            const response = await vendorService.getByVendorName(value);
            console.log("response 000", response);

            if (response?.is_exist === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
      code: yup
      .string()
      .required(vendorErrorMessages.ENTER_CODE)
      .min(4, vendorErrorMessages.MIN_CODE)
      .max(10, vendorErrorMessages.MAX_CODE)
      .test(
        'code-availability',
        vendorErrorMessages.VENDOR_CODE_EXISTS,
        async (value: any) => {
          if (value) {
            const response = await vendorService.getByVendorCode(value);
            if (response?.is_exist === true) return false;
            else return true;
          }
        }
      ),
    contact_person: yup
      .string()
      .max(50, vendorErrorMessages.ENTER_MAX_NAME)
      .required(vendorErrorMessages.ENTER_CONTACTPERSONNAME),
    contact_email: yup
      .string()
      .required(vendorErrorMessages.ENTER_EMAIL)
      .email(vendorErrorMessages.ENTER_VALID_EMAIL)
      .test(
        'email-availability',
        vendorErrorMessages.EMAIL_EXISTS,
        async (value: any) => {
          if (value) {
            const response = await vendorService.getOneVendorEmail(value);
            if (response?.is_exist === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    contact_phone_no: yup
      .string()
      .matches(/^\d{10}$/, vendorErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(vendorErrorMessages.ENTER_MOBILENUMBER),
    address: yup.object().shape({
      city: yup.string().matches(/^[A-Za-z]+$/, 'Invalid city name'),
      state: yup.string().matches(/^[A-Za-z\s]+$/, 'Invalid state name'),
      country: yup.string().matches(/^[A-Za-z]+$/, 'Invalid country name'),
      pin_code: yup
        .number()
        .min(100000, 'PIN code must be a 6-digit number')
        .max(999999, 'PIN code must be a 6-digit number')
        .typeError('Only numbers are allowed'),
    }),
    bank_account_details: yup.object().shape({
      account_no: yup
        .number()
        .required(vendorErrorMessages.ENTER_BANKACCOUNTNUMBER)
        .typeError('Invalid Account number')
        .test(
          'is-valid-number',
          'Account number must have 8 to 16 digits',
          (value: any) => {
            const stringValue = String(value);
            return stringValue.length >= 8 && stringValue.length <= 16;
          }
        ),
      bank_name: yup
        .string()
        .matches(/^[a-zA-Z\s]+$/, vendorErrorMessages.ENTER_ONLY_TEXT),
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
    tax_id: yup
      .string()
      .required(vendorErrorMessages.ENTER_TAX)
      .matches(/^([A-Z]{5}[0-9]{4}[A-Z])$/, 'Invalid Tax number'),
  });
};


export const getVendorEditYupschema = (yup: any) => {
  return yup.object().shape({
    vendor_name: yup
      .string()
      .max(100, vendorErrorMessages.ENTER_MAX_NAME)
      .required(vendorErrorMessages.ENTER_VENDORNAME),
      // .test(
      //   'vendor-name dublicate',
      //   vendorErrorMessages.VENDOR_EXISTS,
      //   async (value: any) => {
      //     if (value) {
      //       const response = await vendorService.getByVendorName(value);
      //       console.log("response 000", response);

      //       if (response?.is_exist === true) {
      //         return false;
      //       } else {
      //         return true;
      //       }
      //     }
      //   }
      // ),
      code: yup
      .string()
      .required(vendorErrorMessages.ENTER_CODE)
      .min(4, vendorErrorMessages.MIN_CODE)
      .max(10, vendorErrorMessages.MAX_CODE),
    contact_person: yup
      .string()
      .max(50, vendorErrorMessages.ENTER_MAX_NAME)
      .required(vendorErrorMessages.ENTER_CONTACTPERSONNAME),
    contact_email: yup
      .string()
      .required(vendorErrorMessages.ENTER_EMAIL)
      .email(vendorErrorMessages.ENTER_VALID_EMAIL),
    // .test(
    //   'email-availability',
    //   vendorErrorMessages.EMAIL_EXISTS,
    //   async (value: any) => {
    //     if (value) {
    //       const response = await vendorService.getOneVendorEmail(value);
    //       console.log('email id check==>', response);
    //       if (response?.is_exist === true) {
    //         return false;
    //       } else {
    //         return true;
    //       }
    //     }
    //   }
    // ),
    contact_phone_no: yup
      .string()
      .matches(/^\d{10}$/, vendorErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(vendorErrorMessages.ENTER_MOBILENUMBER),
    address: yup.object().shape({
      city: yup.string().matches(/^[A-Za-z]+$/, 'Invalid city name'),
      state: yup.string().matches(/^[A-Za-z\s]+$/, 'Invalid state name'),
      country: yup.string().matches(/^[A-Za-z]+$/, 'Invalid country name'),
      pin_code: yup
        .number()
        .min(100000, 'PIN code must be a 6-digit number')
        .max(999999, 'PIN code must be a 6-digit number')
        .typeError('Only numbers are allowed'),
    }),
    bank_account_details: yup.object().shape({
      account_no: yup
        .number()
        .required(vendorErrorMessages.ENTER_BANKACCOUNTNUMBER)
        .typeError('Invalid Account number')
        .test(
          'is-valid-number',
          'Account number must have 8 to 16 digits',
          (value: any) => {
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
    tax_id: yup
      .string()
      .required(vendorErrorMessages.ENTER_TAX)
      .matches(/^([A-Z]{5}[0-9]{4}[A-Z])$/, 'Invalid Tax number'),
  });
};
