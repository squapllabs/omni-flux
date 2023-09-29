export const purchaseRequestAdd = {
  ENTER_NAME: 'Items is required',
  ENTER_QUANTITY: 'Quantity is required',
  TYPE_ERROR: 'Must be a number',
  ENTER_VENDOR: 'Vendor is required',
};

export const getPurchaseRequestCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    item_name: yup.string().required(purchaseRequestAdd.ENTER_NAME),
    quantity: yup.number().typeError(purchaseRequestAdd.TYPE_ERROR),
  });
};
