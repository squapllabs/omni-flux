export const abstractErrorMessages = {
    ENTER_NAME: 'Abstract Name is required',
    ENTER_DESCRIPTION: 'Description is required'
  };
  
  export const getAbstractValidateyup = (yup: any) => {
    return yup.object().shape({
        name: yup
        .string()
        .trim()
        .required(abstractErrorMessages.ENTER_NAME),
        description:yup
        .string()
        .required(abstractErrorMessages.ENTER_DESCRIPTION),
    });
  };
  

  export const subCategoryErrorMessages = {
    ENTER_NAME: 'Abstract Name is required',
    ENTER_DESCRIPTION: 'Description is required'
  };
  
  export const getSubCategoryValidateyup = (yup: any) => {
    return yup.object().shape({
        name: yup
        .string()
        .trim()
        .required(subCategoryErrorMessages.ENTER_NAME),
        description:yup
        .string()
        .required(subCategoryErrorMessages.ENTER_DESCRIPTION),
    });
  };