export const gstErrorMessages = {
    ENTER_RATE: 'Required Field',
    ENTER_RATES: 'Value cannot be a alphabet',
    ENTER_VALID_RATE:'Upto two decimal only allow'
  };

export const getGstcreationYupschema = (yup: any) => {
    return yup.object().shape({
    rate: yup
      .number()
      .typeError(gstErrorMessages.ENTER_RATES)
      .required(gstErrorMessages.ENTER_RATE)
      .positive(gstErrorMessages.ENTER_VALID_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value : number) => !isNaN(value) && value.toString().match(/^\d+(\.\d{1,2})?$/)
      ),
      cgst_rate: yup
      .number()
      .typeError(gstErrorMessages.ENTER_RATES)
      .required(gstErrorMessages.ENTER_RATE)
      .positive(gstErrorMessages.ENTER_VALID_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value : number) => !isNaN(value) && value.toString().match(/^\d+(\.\d{1,2})?$/)
      ),
      igst_rate: yup
      .number()
      .typeError(gstErrorMessages.ENTER_RATES)
      .required(gstErrorMessages.ENTER_RATE)
      .positive(gstErrorMessages.ENTER_VALID_RATE)
      .test(
        "decimal-validation",
        gstErrorMessages.ENTER_VALID_RATE,
        (value : number) => !isNaN(value) && value.toString().match(/^\d+(\.\d{1,2})?$/)
      ),
    //   cgst_rate: yup.number().required(gstErrorMessages.ENTER_CGST_RATE),
    //   igst_rate: yup.number().required(gstErrorMessages.ENTER_IGST_RATE),
    });
  };