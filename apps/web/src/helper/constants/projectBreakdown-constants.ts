// import uomService from '../../service/uom-service';

export const projectBreakDownMessages = {
  ENTER_NAME: 'Work breakdown name is required',
  ENTER_CODE: 'Work breakdown code is required',
  ENTER_TYPE: 'Work breakdown type is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_SPECIAL_CHARACTER: 'Special Characters are not allowed',
  MINIMUM_CHECK: 'Value must be greater than 0',
  MAXIMUM_CHECK: 'Value must be less then 100000',
  TYPE_ERROR: 'Only Number are allowed',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    project_workbreak_down_name: yup
    .string()
    .typeError(projectBreakDownMessages.ENTER_NAME)
    .required(projectBreakDownMessages.ENTER_NAME),
    project_workbreak_down_code: yup
    .string()
    .typeError(projectBreakDownMessages.ENTER_NAME)
    .required(projectBreakDownMessages.ENTER_CODE),
    project_workbreak_down_type: yup
    .string()
    .typeError(projectBreakDownMessages.ENTER_NAME)
    .required(projectBreakDownMessages.ENTER_TYPE),
    rate: yup
    .number()
      .min(1, projectBreakDownMessages.MINIMUM_CHECK)
      .max(100000, projectBreakDownMessages.MAXIMUM_CHECK)
      .typeError(projectBreakDownMessages.TYPE_ERROR),
    // .string()
    // .typeError(projectBreakDownMessages.ENTER_NAME)
    // .required(projectBreakDownMessages.ENTER_TYPE),

})
};