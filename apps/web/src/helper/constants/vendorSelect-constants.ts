export const userErrorMessages = {
    ENTER_BUDGET: 'Budget is required',
    ENTER_RATE_TYPE: 'Must be a number',
}

export const getCreateValidateyup = (yup: any) => {
    return yup.object().shape({
        total_quotation_amount: yup
        .number().required(userErrorMessages.ENTER_BUDGET).typeError(userErrorMessages.ENTER_RATE_TYPE),
    });
}