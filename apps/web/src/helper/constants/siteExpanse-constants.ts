export const siteExpanseMessages = {
  ENTER_NAME: 'Site name is required',
  ENTER_DEPARTMENT: 'Department is required',
  CODE_EXIST: 'Code is already present',
  ENTER_START_DATE: 'Start Date  is required',
  ENTER_END_DATE: 'End Date is required',
  ENTER_PURPOSE: 'Purpose is required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    // employee_name: yup.string().required(siteExpanseMessages.ENTER_NAME),
    // employee_id: yup.string().required(siteExpanseMessages.ENTER_DEPARTMENT),
    // employee_phone: yup
    //   .string()
    //   .matches(/^\d{10}$/, 'Contact number must be a 10 digit number')
    //   .typeError('Invalid contact number'),
    // purpose: yup
    //   .string()
    //   .typeError(siteExpanseMessages.ENTER_PURPOSE)
    //   .required(siteExpanseMessages.ENTER_PURPOSE),
    // department: yup.string().required(siteExpanseMessages.ENTER_DEPARTMENT),
    // start_date: yup
    //   .date()
    //   .typeError(siteExpanseMessages.ENTER_START_DATE)
    //   .required(siteExpanseMessages.ENTER_START_DATE),
    // end_date: yup
    //   .date()
    //   .min(yup.ref('start_date'), 'End date cannot be earlier than start date')
    //   .test(
    //     'is-greater',
    //     'End date must be greater than the start date',
    //     function (value: string | number | Date, { parent }: yup.TestContext) {
    //       const startDate = parent.start_date;
    //       if (!startDate || !value) return true;
    //       return new Date(value) > new Date(startDate);
    //     }
    //   )
    //   .typeError(siteExpanseMessages.ENTER_END_DATE)
    //   .required(siteExpanseMessages.ENTER_END_DATE),
  });
};
