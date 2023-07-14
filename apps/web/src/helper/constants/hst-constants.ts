export const userErrorMessages = {
    ENTER_CODE: 'Code is required',
    ENTER_DESCRIPTION: 'Description is required',
  };
  
  export const gethsnValidateyup = (yup: any) => {
    return yup.object().shape({
      code: yup
        .string()
        .typeError(userErrorMessages.ENTER_CODE)
        .required(userErrorMessages.ENTER_CODE),
      description: yup
        .string()
        .typeError(userErrorMessages.ENTER_DESCRIPTION)
        .required(userErrorMessages.ENTER_DESCRIPTION),
    });
  };
  