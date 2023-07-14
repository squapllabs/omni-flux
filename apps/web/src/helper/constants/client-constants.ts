export const userErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_CONTACTDETAILS: 'contact Detail is required',
};

export const getClientValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME),
    contact_details: yup
      .string()
      .typeError(userErrorMessages.ENTER_CONTACTDETAILS)
      .required(userErrorMessages.ENTER_CONTACTDETAILS),
  });
};
