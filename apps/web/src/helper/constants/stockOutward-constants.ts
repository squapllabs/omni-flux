

export const stockOutwardErrorMessages = {
    ENTER_SITE: 'Site is required',
    ENTER_SITE_ENGINEER: 'Site Engineer Name is required',
    ENTER_ITEM: 'Item Name is required',
    ENTER_QUANTITY: 'Quantity is required',
    TYPE_ERROR: 'Only Number are allowed',
};

export const getStockOutwardCreationYupschema = (yup: any) => {
    return yup.object().shape({
        site_id: yup
            .number()
            .required(stockOutwardErrorMessages.ENTER_SITE),
        site_engineer_id: yup
            .number()
            .required(stockOutwardErrorMessages.ENTER_SITE_ENGINEER),
    });
};

export const getStockOutwardItemCreationYupschema = (yup: any) => {
    return yup.object().shape({
        item_id: yup
            .number()
            .required(stockOutwardErrorMessages.ENTER_ITEM),
        outward_quantity: yup
            .number()
            .required(stockOutwardErrorMessages.ENTER_QUANTITY)
            .typeError(stockOutwardErrorMessages.TYPE_ERROR)
            .max(yup.ref('available_quantity'), 'quantity must be less than or equal to available quantity'),
        available_quantity: yup
            .number()
            // .required()
    });
};