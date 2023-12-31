export const purchaseRequestAdd = {
  ENTER_NAME: 'Items is required',
  ENTER_QUANTITY: 'Quantity is required',
  TYPE_ERROR: 'Numbers only allowed',
  ENTER_VENDOR: 'Vendor is required',
  ENTER_ALLOCATED_QUANTITY: 'Allocated Quantity is required'
};

export const getPurchaseRequestCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    // item_name: yup.string().required(purchaseRequestAdd.ENTER_NAME),
    // purchase_requested_quantity: yup.number()
    //   .required(purchaseRequestAdd.ENTER_ALLOCATED_QUANTITY)
    //   .typeError(purchaseRequestAdd.TYPE_ERROR)
    //   .positive(purchaseRequestAdd.TYPE_ERROR),
    vendor_id: yup.number()
      .required(purchaseRequestAdd.ENTER_VENDOR)
  });
};
