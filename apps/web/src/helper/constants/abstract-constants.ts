export const abstractErrorMessages = {
  ENTER_NAME: 'Abstract Name is required',
  ENTER_DESCRIPTION: 'Description is required',
  END_DATE: 'End Date must be after start date',
};

export const getAbstractValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup.string().trim().required(abstractErrorMessages.ENTER_NAME),
    description: yup.string().required(abstractErrorMessages.ENTER_DESCRIPTION),
    start_date: yup.date(),
    end_date: yup
      .date()
      .min(yup.ref('start_date'), 'End date cannot be earlier than start date')
      .test(
        'is-greater',
        'End date must be greater than the start date',
        function (value: string | number | Date, { parent }: yup.TestContext) {
          const startDate = parent.start_date;
          if (!startDate || !value) return true;
          return new Date(value) > new Date(startDate);
        }
      ),
  });
};

export const subCategoryErrorMessages = {
  ENTER_NAME: 'Task Name is required',
  ENTER_DESCRIPTION: 'Description is required',
};

export const getSubCategoryValidateyup = (yup: any) => {
  return yup.object().shape({
    // name: yup.string().trim().required(subCategoryErrorMessages.ENTER_NAME),
    description: yup
      .string()
      .required(subCategoryErrorMessages.ENTER_DESCRIPTION),
    // start_date: yup.date(),
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
    //   ),
  });
};
