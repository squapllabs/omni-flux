import siteService from '../../service/site-service';

export const siteDownMessages = {
  ENTER_NAME: 'Site name is required',
  ENTER_CODE: 'Site code is required',
  CODE_EXIST: 'Code is already present',
  MIN_CODE: 'Code must be more then 5',
  MAX_CODE: 'Code must lesser then 7',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup.string().required(siteDownMessages.ENTER_NAME),
    // code: yup
    //   .string()
    //   .required(siteDownMessages.ENTER_CODE)
    //   .min(5, siteDownMessages.MIN_CODE)
    //   .max(7, siteDownMessages.MAX_CODE)
    //   .test(
    //     'code-availability',
    //     siteDownMessages.CODE_EXIST,
    //     async (value: any) => {
    //       if (value) {
    //         const response = await siteService.checkSiteCodeDuplicate(value);
    //         console.log('response', response);

    //         if (response?.is_exist === true) return false;
    //         else return true;
    //       }
    //     }
    //   ),
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

export const editCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup.string().required(siteDownMessages.ENTER_NAME),
    code: yup
      .string()
      .required(siteDownMessages.ENTER_CODE)
      .min(5, siteDownMessages.MIN_CODE)
      .max(7, siteDownMessages.MAX_CODE),
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
