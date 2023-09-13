import addProduct from "../../service/add-product";

export const userErrorMessages = {
  ENTER_NAME: 'Item Name is required',
  ENTER_TYPE: 'Item Type is required',
  ALREADY_EXIST: 'The item name already exists.',
  ENTER_GST: 'Gst is required',
  ENTER_UOM: 'Unit of Measurement is required',
  ENTER_HSN: 'HSN Code is required',
  ENTER_DESCRIPTION: 'Description is required',
  NAME_EXIST: 'Item Name already present',
  ENTER_RATE: 'Rate is required',
  ENTER_RATE_TYPE: 'Must be a number',
};

export const getCreateValidateyup = (yup: any) => {
  return yup.object().shape({
    item_name: yup
      .string()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME)
      .test(
        'itemname-availability',
        userErrorMessages.NAME_EXIST,
        async (value: any) => {
          if (value) {
            const response = await addProduct.getOneItemByName(value);
            if (response?.status === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    item_type_id: yup.string().trim().required(userErrorMessages.ENTER_TYPE),
    description: yup.string().required(userErrorMessages.ENTER_DESCRIPTION),
    gst_id: yup.string().required(userErrorMessages.ENTER_GST),
    uom_id: yup.string().required(userErrorMessages.ENTER_UOM),
    hsn_code_id: yup.string().required(userErrorMessages.ENTER_HSN),
    rate: yup.number().required(userErrorMessages.ENTER_RATE).typeError(userErrorMessages.ENTER_RATE_TYPE),
  });
};


export const getUpdateValidateyup = (yup: any) => {
  return yup.object().shape({
    rate: yup.number().required(userErrorMessages.ENTER_RATE).typeError(userErrorMessages.ENTER_RATE_TYPE),
    description: yup.string().required(userErrorMessages.ENTER_DESCRIPTION),
    gst_id: yup.string().required(userErrorMessages.ENTER_GST),
    uom_id: yup.string().required(userErrorMessages.ENTER_UOM),
    hsn_code_id: yup.string().required(userErrorMessages.ENTER_HSN),
  });
};

