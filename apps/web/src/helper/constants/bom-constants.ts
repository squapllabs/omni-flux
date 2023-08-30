export const bomErrorMessages = {
    ENTER_NAME: 'Bom Name is required',
    ENTER_QUANTITY: 'Quantity is required',
    ENTER_ITEM:'Item Name is required',
    ENTER_UOM:'Uom is required',
    TYPECHECK:'Numbers only allowed'
  };
  
  export const getBomValidateyup = (yup: any) => {
    return yup.object().shape({
        bom_name: yup
        .string()
        .trim()
        .required(bomErrorMessages.ENTER_NAME),
        quantity: yup
        .number()
        .required(bomErrorMessages.ENTER_QUANTITY)
        .typeError(bomErrorMessages.TYPECHECK),
        item_id: yup
        .string()
        .trim()
        .required(bomErrorMessages.ENTER_ITEM),
        uom_id: yup
        .string()
      .trim()
        .required(bomErrorMessages.ENTER_UOM),
    });
  };
  