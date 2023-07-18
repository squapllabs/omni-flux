export const userErrorMessages = {
  ENTER_CODE: 'Code is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_NUMBERONLY: 'Number only allowed',
  MIN_CODE: 'Minimum 4 digit needed',
  MAX_CODE: 'Maximum 8 digit needed',
};

export const gethisnValidateyup = (yup: any) => {
  return yup.object().shape({
    code: yup
      .string()
      .typeError(userErrorMessages.ENTER_CODE)
      .required(userErrorMessages.ENTER_CODE)
      .matches(/^[0-9]+$/, userErrorMessages.ENTER_NUMBERONLY)
      .min(4, userErrorMessages.MIN_CODE)
      .max(8, userErrorMessages.MAX_CODE),
    description: yup
      .string()
      .typeError(userErrorMessages.ENTER_DESCRIPTION)
      .required(userErrorMessages.ENTER_DESCRIPTION),
  });
};
