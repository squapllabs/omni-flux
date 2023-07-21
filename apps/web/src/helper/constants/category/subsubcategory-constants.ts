import subSubCategoryService from 'apps/web/src/service/subSubCategory-service';

export const subSubErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_BUDGET: 'Budget is required',
  SELECT_SUB_CATEGORY: 'Select the sub category',
  ALREADY_EXIST:
    'The Sub Sub Category already exists in the same Sub category with the same name.',
};

export const getCreateValidateyup = (yup: any) => {
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
      .required(subSubErrorMessages.ENTER_NAME)
      .test(
        'name-availability',
        subSubErrorMessages.ALREADY_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const sub_category_id = parent.sub_category_id;
          const object = {
            id: sub_category_id,
            name: value,
          };
          if (value) {
            const response =
              await subSubCategoryService.checkDublicateSubSubCategory(object);
            console.log('response', response);
            if (response?.status === true) {
              console.log('false');
              return false;
            } else {
              console.log('true');
              return true;
            }
          }
        }
      ),
    budget: yup.number().required(subSubErrorMessages.ENTER_BUDGET),
  });
};
export const getUpdateValidateyup = (yup: any) => {
  return yup.object().shape({
    sub_sub_category_id: yup.string().trim().required(),
    sub_category_id: yup
      .string()
      .trim()
      .typeError(subSubErrorMessages.SELECT_SUB_CATEGORY)
      .required(subSubErrorMessages.SELECT_SUB_CATEGORY),
    name: yup
      .string()
      .trim()
      .typeError(subSubErrorMessages.ENTER_NAME)
      .required(subSubErrorMessages.ENTER_NAME)
      .test(
        'name-availability',
        subSubErrorMessages.ALREADY_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const sub_category_id = parent.sub_category_id;
          const sub_sub_category_id = parent.sub_sub_category_id;
          const object = {
            id: sub_category_id,
            name: value,
          };
          if (value) {
            const response =
              await subSubCategoryService.checkDublicateSubSubCategory(object);
            if (
              response?.status === true &&
              response.data.sub_sub_category_id === Number(sub_sub_category_id)
            ) {
              return true;
            } else if (response?.status === false) {
              return true;
            } else {
              return false;
            }
          }
        }
      ),
    budget: yup.number().required(subSubErrorMessages.ENTER_BUDGET),
  });
};
