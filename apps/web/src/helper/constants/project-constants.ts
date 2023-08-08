export const projectBreakDownMessages = {
  ENTER_NAME: 'Project name is required',
  ENTER_CODE: 'Project code is required',
  ENTER_USER: 'Project manager is required',
  ENTER_CLIENT: 'Project client/customer is required',
  MINIMUM_CHECK: 'Value must be greater than 0',
  MAXIMUM_CHECK: 'Value must be less then 100000',
  TYPE_ERROR: 'Only Number are allowed',
  TYPE_ACTUAL: 'Actual budget is required',
  TYPE_ESTIMATE: 'Estimated budget is required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    project_name: yup.string().required(projectBreakDownMessages.ENTER_NAME),
    code: yup.string().required(projectBreakDownMessages.ENTER_CODE),
    user_id: yup.string().trim().required(projectBreakDownMessages.ENTER_USER),
    client_id: yup.string().trim().required(projectBreakDownMessages.ENTER_CLIENT),
    estimated_budget: yup
      .number()
      .min(1, projectBreakDownMessages.MINIMUM_CHECK)
      .max(100000, projectBreakDownMessages.MAXIMUM_CHECK)
      .typeError(projectBreakDownMessages.TYPE_ERROR)
      .required(projectBreakDownMessages.TYPE_ESTIMATE),
    actual_budget: yup
      .number()
      .min(1, projectBreakDownMessages.MINIMUM_CHECK)
      .max(100000, projectBreakDownMessages.MAXIMUM_CHECK)
      .typeError(projectBreakDownMessages.TYPE_ERROR)
      .required(projectBreakDownMessages.TYPE_ACTUAL),
  });
};
