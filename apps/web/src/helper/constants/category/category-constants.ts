export const userErrorMessages = {
  ENTER_NAME: 'Name is required',
  ENTER_BUDGET: 'Budget is required',
};

export const getClientValidateyup = (yup: any) => {
  return yup.object().shape({
    name: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_NAME)
      .required(userErrorMessages.ENTER_NAME),
    budget: yup
      .number()
      .required(userErrorMessages.ENTER_BUDGET)
      .test(
        'Is positive?',
        'Budget must be greater than 0!',
        (value: number) => value > 0
      ),
  });
};
