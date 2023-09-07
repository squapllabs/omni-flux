export const bomErrorMessages = {
  ENTER_NAME: 'Bom Name is required',
  ENTER_QUANTITY: 'Quantity is required',
  ENTER_ITEM: 'Item Name is required',
  ENTER_UOM: 'Uom is required',
  TYPECHECK: 'Numbers only allowed',
  ITEM_EXIST: 'Item already exist',
};

export const getBomValidateyup = (yup: any) => {
  return yup.object().shape({
    bom_name: yup.string().trim().required(bomErrorMessages.ENTER_NAME),
    quantity: yup
      .number()
      .required(bomErrorMessages.ENTER_QUANTITY)
      .typeError(bomErrorMessages.TYPECHECK),
    item_id: yup.string().trim().required(bomErrorMessages.ENTER_ITEM),
    uom_id: yup.string().trim().required(bomErrorMessages.ENTER_UOM),
  });
};
export const getBombulkValidateyup = (yup: any) => {
  return yup.object().shape({
    bom_name: yup.string().trim().required(bomErrorMessages.ENTER_NAME),
    quantity: yup
      .number()
      .required(bomErrorMessages.ENTER_QUANTITY)
      .typeError(bomErrorMessages.TYPECHECK),
    item_id: yup
      .string()
      .trim()
      .required(bomErrorMessages.ENTER_ITEM)
      .test(
        'decimal-validation',
        'bomErrorMessages',
        function (value: number, { parent }: yup.TestContext) {
          const list = parent.bom_list;
          console.log('list', list);
          return true;
        }
      ),
    uom_id: yup.string().trim().required(bomErrorMessages.ENTER_UOM),
  });
};
