
export const subSubErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_BUDGET: 'Budget is required',
  SELECT_SUB_CATEGORY: 'Select the sub category',
};

export const getSubSubCategoryValidateyup = (yup: any) => {
  return yup.object().shape({
    sub_category_id: yup
      .string()
      .trim()
      .typeError(subSubErrorMessages.SELECT_SUB_CATEGORY)
      .required(subSubErrorMessages.SELECT_SUB_CATEGORY),
    name: yup
      .string()
      .trim()
      .typeError(subSubErrorMessages.ENTER_NAME)
      .required(subSubErrorMessages.ENTER_NAME),
    budget: yup.number().required(subSubErrorMessages.ENTER_BUDGET),
  });
};
