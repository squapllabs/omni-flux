export const gstErrorMessages = {
    ENTER_RATE: 'Required Field',
    TYPE_ERROR: 'Value cannot be a alphabet',
    ENTER_VALID_RATE:'check your value',
  };

export const getGstcreationYupschema = (yup: any) => {
    return yup.object().shape({
    rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value: number) => {
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return !isNaN(value) && decimalPattern.test(value.toString());
        }
      ),
      cgst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value: number) => {
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return !isNaN(value) && decimalPattern.test(value.toString());
        }
      ),
      igst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value: number) => {
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return !isNaN(value) && decimalPattern.test(value.toString());
        }
      ),
    });
  };