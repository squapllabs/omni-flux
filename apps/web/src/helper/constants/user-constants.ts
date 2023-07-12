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
  ENTER_FIRSTNAME: 'First name is required',
  ENTER_USERNAME: 'Username is required',
  ENTER_LASTNAME: 'Last name is required',
  ENTER_MOBILENUMBER: 'Mobile number is required',
  ENTER_VALID_MOBILENUMBER: 'Invalid mobile number',
  ENTER_GENDER: 'Gender is required',
  SELECT_USERSTATUS: 'User status is required',
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
export const getUsercreationYupschema = (yup: any) => {
  return yup.object().shape({
    first_name: yup.string().required(userErrorMessages.ENTER_FIRSTNAME),
    last_name: yup.string().required(userErrorMessages.ENTER_LASTNAME),
    contact_no: yup.string().required(userErrorMessages.ENTER_MOBILENUMBER),
    email_id: yup
      .string()
      .required(userErrorMessages.ENTER_EMAIL)
      .email(userErrorMessages.ENTER_VALID_EMAIL),
    user_password: yup
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
    user_status: yup.string().required(userErrorMessages.SELECT_USERSTATUS),
    // address: yup.object().shape({
    //   state: yup.string().required('State is required'),
    //   area: yup.string().required('Area is required'),
    // }),
  });
};
