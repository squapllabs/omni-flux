export const gstErrorMessages = {
  ENTER_RATE: 'Required Field',
  TYPE_ERROR: 'Value cannot be a string',
  ENTER_VALID_RATE: 'check your value',
};

export const getGstcreationYupschema = (yup: any) => {
  return yup.object().shape({
    rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
      .integer(gstErrorMessages.ENTER_VALID_RATE)
      .min(0, gstErrorMessages.ENTER_VALID_RATE)
      .max(99, gstErrorMessages.ENTER_VALID_RATE),


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
          console.log("sgst value ====>",value)
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
        gstErrorMessages.ENTER_VALID_RATE,
        function (value: number, { parent }: yup.TestContext) {
          if (value === undefined || value === null) {
            return true;
          }
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          console.log("cgst_====>",value)
          console.log("inside value sgst_rate===>",parent?.sgst_rate)
          console.log("rate=>",parent?.rate)
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
        gstErrorMessages.ENTER_VALID_RATE,
        (value: number, { parent } :  yup.TestContext) => {
          if (value === undefined || value === null) {
            return true; // Allow empty values
          }
          const decimalPattern = /^\d{1,2}(\.\d{1,2})?$/;
          return (
            !isNaN(value) &&
            decimalPattern.test(value.toString()) &&
            (value  === parent?.rate)
          );
        }
      ),
  });
};
