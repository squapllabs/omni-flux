import categoryService from 'apps/web/src/service/category-service';

export const userErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_BUDGET: 'Budget is required',
  SELECT_CATEGORY: 'Please Select the category',
};

export const getSubcategoryValidateyup = (yup: any) => {
  return yup.object().shape({
    category_id: yup
      .string()
      .trim()
      .typeError(userErrorMessages.SELECT_CATEGORY)
      .required(userErrorMessages.SELECT_CATEGORY),
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME),
    budget: yup.number().required(userErrorMessages.ENTER_BUDGET),
    // .test(
    //   'Is positive?',
    //   'Budget must be greater than 0!',
    //   (value: number) => value > 0
    // )
    // .test(
    //   'code-availability',
    //   'budget is more then the limit',
    //   async (value: any, { parent }: yup.TestContext) => {
    //     if (value) {
    //       console.log('parent', parent.category_id);
    //       const response = await categoryService.getOneCategoryByID(
    //         parent.category_id
    //       );
    //       console.log('responsedemo', response);
    //       if (response?.success === true && value <= response?.data.budget) {
    //         return true;
    //       } else {
    //         return false;
    //       }
    //     }
    //   }
    // ),
  });
};
