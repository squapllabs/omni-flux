export const gstErrorMessages = {
  ENTER_RATE: 'Gst Rate is required',
  TYPE_ERROR: 'Characters not allowed',
  DECIMAL_CHECK: 'Decimal not allowed',
  MAXIMUM_CHECK: 'Value should be 0 to 99',
  ENTER_VALID_RATE: 'check your value',
  MISMATCH_ERROR : 'Please ensure Sgst + Cgst equals GST',
  IGST_ERROR : 'Please ensure Sgst + Cgst equals Igst'
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
          return (
            !isNaN(value) && decimalPattern.test(value.toString())
          );
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
            (value + parent.sgst_rate === parent.rate)
          );
        }
      ),
    igst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .test(
        'decimal-validation',
        gstErrorMessages.IGST_ERROR,
        (value: number, { parent } :  yup.TestContext) => {
          if (value === undefined || value === null) {
            return true;
          }
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return (
            !isNaN(value) &&
            decimalPattern.test(value.toString()) &&
            (value  === parent?.rate || value === 0)
          );
        }
      ),
  });
};
