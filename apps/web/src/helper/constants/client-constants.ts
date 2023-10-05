import clientService from '../../service/client-service';

export const userErrorMessages = {
  ENTER_NAME: 'Client Name is required',
  ENTER_MAX_NAME: 'Name should not exceed 100 characters',
  NAME_EXISTS: 'Client Name is already present',
  ENTER_MOBILENUMBER: 'Contact number is required',
  ENTER_VALID_MOBILENUMBER: 'Invalid mobile number',
};

export const getClientValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .test(
        'client-availability',
        userErrorMessages.NAME_EXISTS,
        async (value: any) => {
          const response = await clientService.getOneClientByName(value);
          if (response?.status === true) {
            return false;
          } else {
            return true;
          }
        }
      ),
    contact_details: yup
      .string()
      .matches(/^\d{10}$/, userErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(userErrorMessages.ENTER_MOBILENUMBER),
  });
};

export const getUpdateClientValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .test(
        'client-availability',
        userErrorMessages.NAME_EXISTS,
        async (value: any) => {
          const response = await clientService.getOneClientByName(value);
          console.log('response', response);

          if (response?.status === false) {
            return false;
          } else {
            return true;
          }
        }
      ),
    contact_details: yup
      .string()
      .matches(/^\d{10}$/, userErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(userErrorMessages.ENTER_MOBILENUMBER),
  });
};
