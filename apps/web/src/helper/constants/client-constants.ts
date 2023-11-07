import clientService from '../../service/client-service';

export const userErrorMessages = {
  ENTER_NAME: 'Client Name is required',
  ENTER_MIN_NAME: 'Name should contain minimum 3 characters',
  ENTER_MAX_NAME: 'Name should not exceed 100 characters',
  NAME_EXISTS: 'Client Name is already present',
  ENTER_ONLY_TEXT: 'Only characters are allowed',
  ENTER_MOBILENUMBER: 'Contact number is required',
  ENTER_VALID_MOBILENUMBER: 'Invalid mobile number',
};

export const getClientValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .matches(/^[\w\s]*$/, userErrorMessages.ENTER_ONLY_TEXT)
      .typeError(userErrorMessages.ENTER_NAME)
      .min(3, userErrorMessages.ENTER_MIN_NAME)
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
      .matches(/^[\w\s]*$/, userErrorMessages.ENTER_ONLY_TEXT)
      .typeError(userErrorMessages.ENTER_NAME)
      .min(3, userErrorMessages.ENTER_MIN_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .test(
        'client-availability',
        userErrorMessages.NAME_EXISTS,
        async (value: any, { parent }: yup.TestContext) => {
          const clientId = parent.client_id;
          if (value) {
            const response = await clientService.getOneClientByName(value);
            if (
              response?.is_exist === true &&
              response?.data[0].client_id === clientId
            ) {
              return true;
            } else {
              if (response?.is_exist === false) {
                return true;
              } else {
                return false;
              }
            }
          }
        }
      ),
    contact_details: yup
      .string()
      .matches(/^\d{10}$/, userErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(userErrorMessages.ENTER_MOBILENUMBER),
  });
};
