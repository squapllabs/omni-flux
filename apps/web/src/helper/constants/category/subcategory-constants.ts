import SubcategoryService from '../../../service/subCategory-service';

export const subCategoryErrorMessages = {
  ENTER_NAME: 'Subcategory name is required',
  ENTER_BUDGET: 'Budget is required',
  SELECT_CATEGORY: 'Please Select the category',
  ALREADY_EXIST:
    'The Sub category already exists in the same category with the same name.',
  MINIMUM_CHECK: 'Value must be greater than 0',
  MAXIMUM_CHECK: 'Value must be less then 100000',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    category_id: yup
      .string()
      .trim()
      .typeError(subCategoryErrorMessages.SELECT_CATEGORY)
      .required(subCategoryErrorMessages.SELECT_CATEGORY),
    name: yup
      .string()
      .trim()
      .typeError(subCategoryErrorMessages.ENTER_NAME)
      .required(subCategoryErrorMessages.ENTER_NAME)
      .test(
        'name-availability',
        subCategoryErrorMessages.ALREADY_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const category_id = parent.category_id;
          console.log('project_id', category_id);
          const object = {
            id: category_id,
            name: value,
          };
          if (value) {
            const response = await SubcategoryService.checkDublicateSubCategory(
              object
            );
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
    budget: yup
      .number()
      .required(subCategoryErrorMessages.ENTER_BUDGET)
      .min(1, subCategoryErrorMessages.MINIMUM_CHECK)
      .max(100000, subCategoryErrorMessages.MAXIMUM_CHECK)
      .typeError(subCategoryErrorMessages.TYPE_ERROR),
  });
};
export const getUpdateValidateyup = (yup: any) => {
  return yup.object().shape({
    sub_category_id: yup.string().trim().required(),
    category_id: yup
      .string()
      .trim()
      .typeError(subCategoryErrorMessages.SELECT_CATEGORY)
      .required(subCategoryErrorMessages.SELECT_CATEGORY),
    name: yup
      .string()
      .trim()
      .typeError(subCategoryErrorMessages.ENTER_NAME)
      .required(subCategoryErrorMessages.ENTER_NAME)
      .test(
        'name-availability',
        subCategoryErrorMessages.ALREADY_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const category_id = parent.category_id;
          const sub_category_id = parent.sub_category_id;
          const object = {
            id: category_id,
            name: value,
          };
          if (value) {
            const response = await SubcategoryService.checkDublicateSubCategory(
              object
            );
            if (
              response?.status === true &&
              response.data.sub_category_id === Number(sub_category_id)
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
    budget: yup
      .number()
      .required(subCategoryErrorMessages.ENTER_BUDGET)
      .test(
        'Is positive?',
        'Budget must be greater than 0!',
        (value: number) => value > 0
      ),
  });
};
