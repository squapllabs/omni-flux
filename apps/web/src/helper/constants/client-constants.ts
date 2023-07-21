export const userErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_CONTACTDETAILS: 'Contact Detail is required',
  ENTER_MAX_NAME: 'Name should not exceed 100 characters'
};

export const getClientValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_NAME),
    contact_details: yup
      .string()
      .typeError(userErrorMessages.ENTER_CONTACTDETAILS)
      .required(userErrorMessages.ENTER_CONTACTDETAILS),
  });
};
