export const gstErrorMessages = {
  ENTER_RATE: 'GST Rate is mandatory',
  TYPE_ERROR: 'Only Number are allowed',
  DECIMAL_CHECK: 'Decimal values are not allowed',
  MAXIMUM_CHECK: 'Number should be between 0 to 99',
  MISMATCH_ERROR: 'SGST Rate + CGST Rate should be equal to GST Rate',
  IGST_ERROR: 'IGST should be equal to GST Rate',
  ENTER_VALID_RATE: 'Check your values',
};

export const getGstcreationYupschema = (yup: any) => {
  return yup.object().shape({
    rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
      .integer(gstErrorMessages.DECIMAL_CHECK)
      .min(0, gstErrorMessages.MAXIMUM_CHECK)
      .max(99, gstErrorMessages.MAXIMUM_CHECK),

    sgst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .test(
        'decimal-validation',
        gstErrorMessages.ENTER_VALID_RATE,
        function (value: number) {
          if (value === undefined || value === null) {
            return true;
          }
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return !isNaN(value) && decimalPattern.test(value.toString());
        }
      ),
    cgst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .test(
        'decimal-validation',
        gstErrorMessages.MISMATCH_ERROR,
        function (value: number, { parent }: yup.TestContext) {
          if (value === undefined || value === null) {
            return true;
          }
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return (
            !isNaN(value) &&
            decimalPattern.test(value.toString()) &&
            value + parent.sgst_rate === parent.rate
          );
        }
      ),
    igst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .test(
        'decimal-validation',
        gstErrorMessages.IGST_ERROR,
        (value: number, { parent }: yup.TestContext) => {
          if (value === undefined || value === null) {
            return true;
          }
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return (
            !isNaN(value) &&
            decimalPattern.test(value.toString()) &&
            (value === parent?.rate || value === 0)
          );
        }
      ),
  });
};
