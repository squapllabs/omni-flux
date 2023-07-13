export const userErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_DESCRIPTION: 'Description is required',
};

export const getuomValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME),
    description: yup
      .string()
      .typeError(userErrorMessages.ENTER_DESCRIPTION)
      .required(userErrorMessages.ENTER_DESCRIPTION),
  });
};
