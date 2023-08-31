export const abstractErrorMessages = {
    ENTER_NAME: 'Abstract Name is required',
    ENTER_QUANTITY: 'Budget is required',
    TYPECHECK:'Numbers only allowed',
    ENTER_DESCRIPTION: 'Description is required'
  };
  
  export const getAbstractValidateyup = (yup: any) => {
    return yup.object().shape({
        name: yup
        .string()
        .trim()
        .required(abstractErrorMessages.ENTER_NAME),
        budget: yup
        .number()
        .required(abstractErrorMessages.ENTER_QUANTITY)
        .typeError(abstractErrorMessages.TYPECHECK),
        description:yup
        .string()
        .required(abstractErrorMessages.ENTER_DESCRIPTION),
    });
  };
  