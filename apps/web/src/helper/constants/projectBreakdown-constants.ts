import projectBreakDownService from '../../service/projectBreakdown-service';

export const projectBreakDownMessages = {
  ENTER_NAME: 'Work breakdown name is required',
  ENTER_CODE: 'Work breakdown code is required',
  ENTER_TYPE: 'Work breakdown type is required',
  ENTER_DESCRIPTION: 'Description is required',
  ENTER_SPECIAL_CHARACTER: 'Special Characters are not allowed',
  MINIMUM_CHECK: 'Value must be greater than 0',
  MAXIMUM_CHECK: 'Value must be less then 100000',
  TYPE_ERROR: 'Only Number are allowed',
  CODE_EXIST: 'Code is already present',
  MIN_CODE: 'Code must be more then 5',
  MAX_CODE: 'Code must lesser then 7',
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
      .required(projectBreakDownMessages.ENTER_CODE)
      .min(3, projectBreakDownMessages.MIN_CODE)
      .max(15, projectBreakDownMessages.MAX_CODE),
    project_workbreak_down_type: yup
      .string()
      .typeError(projectBreakDownMessages.ENTER_NAME)
      .required(projectBreakDownMessages.ENTER_TYPE),
    rate: yup
      .number()
      .min(1, projectBreakDownMessages.MINIMUM_CHECK)
      .max(100000, projectBreakDownMessages.MAXIMUM_CHECK)
      .typeError(projectBreakDownMessages.TYPE_ERROR),
  });
};
