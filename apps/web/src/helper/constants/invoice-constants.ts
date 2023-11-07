

export const InvoiceErrorMessages = {
    ENTER_METHOD: 'Payment method is required'
  };
  
  
  export const editInvoiceValidateyup = (yup: any) => {
    return yup.object().shape({
        payment_mode: yup
        .string()
        .required(InvoiceErrorMessages.ENTER_METHOD),
    });
  };
  
  
  
  