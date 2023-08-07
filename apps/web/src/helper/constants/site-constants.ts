// import projectBreakDownService from '../../service/projectBreakdown-service';

export const siteDownMessages = {
  ENTER_NAME: 'Site name is required',
  ENTER_CODE: 'Site code is required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup.string().required(siteDownMessages.ENTER_NAME),
    contact_number: yup
      .string()
      .matches(/^\d{10}$/, 'Contact number must be a 10 digit number')
      .typeError('Invalid contact number'),
    mobile_number: yup
      .string()
      .matches(/^\d{10}$/, 'Mobile number must be a 10 digit number')
      .typeError('Invalid mobile number'),
    address: yup.object().shape({
      city: yup.string().matches(/^[A-Za-z]+$/, 'Invalid city name'),
      state: yup.string().matches(/^[A-Za-z\s]+$/, 'Invalid state name'),
      country: yup.string().matches(/^[A-Za-z]+$/, 'Invalid country name'),
      pin_code: yup
        .string()
        .matches(/^[0-9]{6}$/, 'Pin code must be a 6-digit number')
        .typeError('Only numbers are allowed'),
    }),
  });
};