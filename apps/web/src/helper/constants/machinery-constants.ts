export const userErrorMessages = {
  ENTER_NAME: 'Machinery name is required',
  ENTER_TYPE: 'Machinery type is required',
  ENTER_RATE: 'Rate is required',
  ENTER_RATE_NO: 'Rate must be a number',
  ENTER_UOM: 'UOM is Required',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    machinery_name: yup
      .string()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME),
    machinery_type: yup
      .string()
      .typeError(userErrorMessages.ENTER_TYPE)
      .required(userErrorMessages.ENTER_TYPE),
    rate: yup
      .number()
      .typeError(userErrorMessages.ENTER_RATE_NO)
      .required(userErrorMessages.ENTER_RATE),
    uom_id: yup
      .number()
      .typeError(userErrorMessages.ENTER_UOM)
      .required(userErrorMessages.ENTER_UOM),
    date_of_purchase: yup.date(),
    warranty_expired_on: yup
      .date()
      .min(yup.ref('date_of_purchase'), 'End date cannot be earlier than start date')
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
