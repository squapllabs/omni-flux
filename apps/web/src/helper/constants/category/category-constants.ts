import categoryService from 'apps/web/src/service/category-service';

export const userErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_BUDGET: 'Budget is required',
  ALREADY_EXIST:
    'The category already exists in the same project with the same name.',
  SELECT_PROJECT_ID: 'Project id required',
  MAXIMUM_CHECK: 'Value must be greater than 0',
  TYPE_ERROR: 'Only Number are allowed',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    project_id: yup
      .string()
      .trim()
      .typeError(userErrorMessages.SELECT_PROJECT_ID),
      // .required(userErrorMessages.SELECT_PROJECT_ID),
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .test(
        'name-availability',
        userErrorMessages.ALREADY_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const project_id = parent.project_id;
          const object = {
            id: project_id,
            name: value,
          };
          if (value) {
            const response = await categoryService.checkDublicateCategory(
              object
            );
            if (response?.status === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    budget: yup
      .number()
      .required(userErrorMessages.ENTER_BUDGET)
      .min(1, userErrorMessages.MAXIMUM_CHECK)
      .typeError(userErrorMessages.TYPE_ERROR)
      // .test(
      //   'Is positive?',
      //   'Budget must be greater than 0!',
      //   (value: number) => value > 0
      // ),
  });
};
export const getUpdateValidateyup = (yup: any) => {
  return yup.object().shape({
    category_id: yup.string().trim().required(),
    project_id: yup
      .string()
      .trim()
      .typeError(userErrorMessages.SELECT_PROJECT_ID)
      .required(userErrorMessages.SELECT_PROJECT_ID),
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .test(
        'name-availability',
        userErrorMessages.ALREADY_EXIST,
        async (value: any, { parent }: yup.TestContext) => {
          const project_id = parent.project_id;
          const category_id = parent.category_id;
          const object = {
            id: project_id,
            name: value,
          };
          if (value) {
            const response = await categoryService.checkDublicateCategory(
              object
            );
            if (
              response?.status === true &&
              response.data?.category_id === Number(category_id)
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
      .required(userErrorMessages.ENTER_BUDGET)
      .test(
        'Is positive?',
        'Budget must be greater than 0!',
        (value: number) => value > 0
      ),
  });
};
