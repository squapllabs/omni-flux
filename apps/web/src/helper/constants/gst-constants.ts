export const gstErrorMessages = {
    ENTER_RATE: 'Required Field',
    TYPE_ERROR: 'Value cannot be a alphabet',
    ENTER_VALID_RATE:'Upto two decimal only allow'
  };

export const getGstcreationYupschema = (yup: any) => {
    return yup.object().shape({
    rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
    //   .positive(gstErrorMessages.ENTER_VALID_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value: number) => !isNaN(value) && value.toString().match(/^\d+(\.\d{1,2})?$/)
      ),
      cgst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
    //   .positive(gstErrorMessages.ENTER_VALID_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value: number) => !isNaN(value) && value.toString().match(/^\d+(\.\d{1,2})?$/)
      ),
      igst_rate: yup
      .number()
      .typeError(gstErrorMessages.TYPE_ERROR)
      .required(gstErrorMessages.ENTER_RATE)
    //   .positive(gstErrorMessages.ENTER_VALID_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value: number) => !isNaN(value) && value.toString().match(/^\d+(\.\d{1,2})?$/)
      ),
    });
  };