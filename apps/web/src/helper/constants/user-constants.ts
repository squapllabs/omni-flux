export const userErrorMessages = {
  ENTER_EMAIL: 'Please enter your email',
  ENTER_PASSWORD: 'Please enter your password',
  ENTER_VALID_EMAIL: 'Please enter a valid email',
  MIN_PASSWORD_LENGTH: 'Password should be at least 8 characters',
  INVALID_LOGIN: 'Invalid username or password',
  EMAIL_NOT_FOUND: "This email doesn't exist. Please register to continue",
  PASSWORD_MATCH: 'Please enter the same passwords',
  PASSWORD_MUST_CONTAIN_ONEUPPERCASER: 'Must Contain  One Uppercase',
  PASSWORD_MUST_CONTAIN_ONELOWERCASER: 'Must Contain  One Lowercase',
  PASSWORD_MUST_CONTAIN_ONESPECIAL: 'Must Contain One Special Case Character',
  PASSWORD_MUST_CONTAIN_ONENUMBER: 'Must Contain one Number',
};

export const getLoginYupSchema = (yup: any) => {
  return yup.object().shape({
    email: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_EMAIL)
      .required(userErrorMessages.ENTER_EMAIL)
      .email(userErrorMessages.ENTER_VALID_EMAIL),
    password: yup
      .string()
      .min(8, userErrorMessages.MIN_PASSWORD_LENGTH)
      .matches(
        /^(?=.*[a-z])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONELOWERCASER
      )
      .matches(
        /^(?=.*[A-Z])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONEUPPERCASER
      )
      .matches(
        /^(?=.*[!@#\$%\^&\*])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONESPECIAL
      )
      .matches(
        /^(?=.*[0-9])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONENUMBER
      )
      .typeError(userErrorMessages.ENTER_PASSWORD)
      .required(userErrorMessages.ENTER_PASSWORD),
  });
};

export const getForgetPasswordYupSchema = (yup: any) => {
  return yup.object().shape({
    new_password: yup
      .string()
      .min(8, userErrorMessages.MIN_PASSWORD_LENGTH)
      .matches(
        /^(?=.*[a-z])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONELOWERCASER
      )
      .matches(
        /^(?=.*[A-Z])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONEUPPERCASER
      )
      .matches(
        /^(?=.*[!@#\$%\^&\*])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONESPECIAL
      )
      .matches(
        /^(?=.*[0-9])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONENUMBER
      )
      .typeError(userErrorMessages.ENTER_PASSWORD)
      .required(userErrorMessages.ENTER_PASSWORD),
    confirm_password: yup
      .string()
      .oneOf([yup.ref('new_password'), null], userErrorMessages.PASSWORD_MATCH),
  });
};
