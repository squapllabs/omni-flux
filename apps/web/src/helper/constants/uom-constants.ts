export const userErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_SPECIAL_CHARACTER: 'Special Characters are not allowed',
};

export const getuomValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .matches(/^[a-zA-Z0-9]+$/, userErrorMessages.ENTER_SPECIAL_CHARACTER),
    description: yup
      .string()
      .typeError(userErrorMessages.ENTER_DESCRIPTION)
      .required(userErrorMessages.ENTER_DESCRIPTION),
  });
};
