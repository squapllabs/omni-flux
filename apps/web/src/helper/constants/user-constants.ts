import userService from "../../service/user-service";

export const userErrorMessages = {
  ENTER_EMAIL: 'Username is required',
  ENTER_PASSWORD: 'Password is required',
  ENTER_VALID_EMAIL: 'Please enter a valid Username',
  MIN_PASSWORD_LENGTH: 'Password should be at least 8 characters',
  INVALID_LOGIN: 'Invalid username or password',
  EMAIL_NOT_FOUND: "This Username doesn't exist. Please register to continue",
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
  SELECT_USERROLE: 'User role is required',
  SELECT_DEPARTMENT: 'Department is required',
  ENTER_VALID_NAME: 'Invalid name',
  ENTER_VALID_DEPARTMENT: 'Invalid department',
  ENTER_MAX_NAME: 'Name should not exceed 100 characters',
  ENTER_MAX_DEPARTMENT: 'Department should not exceed 100 characters',
  ENTER_ROLE: 'Role is required',
  EMAIL_EXISTS: 'Email ID already exists'
};

export const getLoginYupSchema = (yup: any) => {
  return yup.object().shape({
    email: yup
      .string()
      .trim()
      .typeError(userErrorMessages.ENTER_EMAIL)
      .required(userErrorMessages.ENTER_EMAIL)
      .email(userErrorMessages.ENTER_VALID_EMAIL)
      .matches(/^[^\s@]+@[^\s@]+\.[^\s@]+$/,userErrorMessages.ENTER_VALID_EMAIL),
    password: yup
      .string()
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
        /^(?=.*[!@#$%^&*])/,
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
    first_name: yup
      .string()
      .matches(/^[A-Za-z]+$/, userErrorMessages.ENTER_VALID_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_FIRSTNAME),
    last_name: yup
      .string()
      .matches(/^[A-Za-z]+$/, userErrorMessages.ENTER_VALID_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_LASTNAME),
    contact_no: yup
      .string()
      .matches(/^\d{10}$/, userErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(userErrorMessages.ENTER_MOBILENUMBER),
    email_id: yup
      .string()
      .required(userErrorMessages.ENTER_EMAIL)
      .email(userErrorMessages.ENTER_VALID_EMAIL)
      .test(
        'email-availability',
        userErrorMessages.EMAIL_EXISTS,
        async(value:any) => {
          if(value) {
            const response = await userService.getOneUser(value);
            if(response?.status === true) {
              return false;
            } else {
              return true;
            }
          }
        }
      ),
    role_id: yup
      .string()
      .required(userErrorMessages.ENTER_ROLE),
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
        /^(?=.*[!@#$%^&*])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONESPECIAL
      )
      .matches(
        /^(?=.*[0-9])/,
        userErrorMessages.PASSWORD_MUST_CONTAIN_ONENUMBER
      )
      .typeError(userErrorMessages.ENTER_PASSWORD)
      .required(userErrorMessages.ENTER_PASSWORD),
    // role_id: yup.string().required(userErrorMessages.SELECT_USERROLE),
    department: yup
      .string()
      .matches(/^[A-Za-z\s]+$/, userErrorMessages.ENTER_VALID_DEPARTMENT)
      .max(100, userErrorMessages.ENTER_MAX_DEPARTMENT)
      .required(userErrorMessages.SELECT_DEPARTMENT),
    address: yup.object().shape({
      city: yup.string().matches(/^[A-Za-z]+$/, 'Invalid city name'),
      state: yup.string().matches(/^[A-Za-z\s]+$/, 'Invalid state name'),
      country: yup.string().matches(/^[A-Za-z]+$/, 'Invalid country name'),
      pin_code: yup.number().typeError('Only numbers are allowed'),
    }),
  });
};
export const getUsereditYupschema = (yup: any) => {
  return yup.object().shape({
    first_name: yup
      .string()
      .matches(/^[A-Za-z]+$/, userErrorMessages.ENTER_VALID_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_FIRSTNAME),
    last_name: yup
      .string()
      .matches(/^[A-Za-z]+$/, userErrorMessages.ENTER_VALID_NAME)
      .max(100, userErrorMessages.ENTER_MAX_NAME)
      .required(userErrorMessages.ENTER_LASTNAME),
    contact_no: yup
      .string()
      .matches(/^\d{10}$/, userErrorMessages.ENTER_VALID_MOBILENUMBER)
      .required(userErrorMessages.ENTER_MOBILENUMBER),
    email_id: yup
      .string()
      .required(userErrorMessages.ENTER_EMAIL)
      .email(userErrorMessages.ENTER_VALID_EMAIL),
    role_id: yup.string().required(userErrorMessages.SELECT_USERROLE),
    department: yup
      .string()
      .matches(/^[A-Za-z\s]+$/, userErrorMessages.ENTER_VALID_DEPARTMENT)
      .max(100, userErrorMessages.ENTER_MAX_DEPARTMENT)
      .required(userErrorMessages.SELECT_DEPARTMENT),
    address: yup.object().shape({
      city: yup.string().matches(/^[A-Za-z]+$/, 'Invalid city name'),
      state: yup.string().matches(/^[A-Za-z\s]+$/, 'Invalid state name'),
      country: yup.string().matches(/^[A-Za-z]+$/, 'Invalid country name'),
      pin_code: yup.number().typeError('Only numbers are allowed'),
    }),
  });
};
