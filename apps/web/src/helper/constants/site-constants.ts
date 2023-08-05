// import projectBreakDownService from '../../service/projectBreakdown-service';

export const siteDownMessages = {
  ENTER_NAME: 'Site name is required',
  ENTER_CODE: 'Site code is required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .typeError(siteDownMessages.ENTER_NAME)
      .required(siteDownMessages.ENTER_NAME),
  });
};

// export const editCreateValidateyup = (yup: any) => {
//   return yup.object().shape({
//     project_workbreak_down_name: yup
//       .string()
//       .typeError(projectBreakDownMessages.ENTER_NAME)
//       .required(projectBreakDownMessages.ENTER_NAME),
//     project_workbreak_down_type: yup
//       .string()
//       .typeError(projectBreakDownMessages.ENTER_NAME)
//       .required(projectBreakDownMessages.ENTER_TYPE),
//     rate: yup
//       .number()
//       .min(1, projectBreakDownMessages.MINIMUM_CHECK)
//       .max(100000, projectBreakDownMessages.MAXIMUM_CHECK)
//       .typeError(projectBreakDownMessages.TYPE_ERROR),
//   });
// };
