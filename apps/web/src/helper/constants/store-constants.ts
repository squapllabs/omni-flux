export const userErrorMessages = {
  ENTER_NAME: 'Store name is required',
  ENTER_MANAGER_NAME: 'Store manager name is required',
  ENTER_EMAIL: 'Store email is required',
  ENTER_VALID_EMAIL: 'Please enter a valid Username',
  ENTER_MOBILENUMBER: 'Store phone number is required',
  ENTER_VALID_MOBILENUMBER: 'Invalid mobile number',
  ENTER_PROJECT: 'Project is required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    project_id: yup
      .string()
      .typeError(userErrorMessages.ENTER_PROJECT)
      .required(userErrorMessages.ENTER_PROJECT),
    store_name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME),
    store_manager_id: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_MANAGER_NAME)
      .required(userErrorMessages.ENTER_MANAGER_NAME),
      contact_email: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_EMAIL)
      .required(userErrorMessages.ENTER_EMAIL)
      .email(userErrorMessages.ENTER_VALID_EMAIL)
      .matches(
        /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
        userErrorMessages.ENTER_VALID_EMAIL
      ),
      contact_phone: yup
      .string()
      .typeError(userErrorMessages.ENTER_MOBILENUMBER)
      .matches(/^\d{10}$/, userErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(userErrorMessages.ENTER_MOBILENUMBER),
  });
};
