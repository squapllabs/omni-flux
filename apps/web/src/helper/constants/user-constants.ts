export const userErrorMessages = {
    ENTER_EMAIL: "Please enter your email",
    ENTER_PASSWORD: "Please enter your password",
    ENTER_VALID_EMAIL: "Please enter a valid email",
    MIN_PASSWORD_LENGTH: "Password should be at least 6 characters",
    INVALID_LOGIN: "Invalid username or password",
    EMAIL_NOT_FOUND: "This email doesn't exist. Please register to continue",
    PASSWORD_MATCH: "Please enter the same passwords"
  };
  
  export const getGender = {
    F: "Female",
    M: "Male",
    O: "Others",
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
        .typeError(userErrorMessages.ENTER_PASSWORD)
        .required(userErrorMessages.ENTER_PASSWORD),
    });
  };