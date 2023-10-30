import siteService from '../../service/site-service';

export const contractorDownMessages = {
  ENTER_NAME: 'Contractor name is required',
  ENTER_CODE: 'Contractor code is required',
  CODE_EXIST: 'Code is already present',
  MIN_CODE: 'Code must be more then 5',
  MAX_CODE: 'Code must lesser then 7',
  ENTER_DESCRIPTION: 'Description is Required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup.string().required(contractorDownMessages.ENTER_NAME),
    code: yup
      .string()
      .required(contractorDownMessages.ENTER_CODE)
      .min(5, contractorDownMessages.MIN_CODE)
      .max(7, contractorDownMessages.MAX_CODE)
      .test(
        'code-availability',
        contractorDownMessages.CODE_EXIST,
        async (value: any) => {
          
          if (value) {
            const response = await siteService.checkSiteCodeDuplicate(value);            
            if (response?.is_exist === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
      description: yup
      .string()
      .trim()
      .required(contractorDownMessages.ENTER_DESCRIPTION),
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
    name: yup.string().required(contractorDownMessages.ENTER_NAME),
    code: yup
      .string()
      .required(contractorDownMessages.ENTER_CODE)
      .min(5, contractorDownMessages.MIN_CODE)
      .max(7, contractorDownMessages.MAX_CODE),
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