import React, { useState, ChangeEvent } from 'react';
import Styles from '../styles/login.module.scss';
import * as yup from 'yup';
import { getLoginYupSchema } from '../helper/constants/user-constants';
import { useLoginAuth, useForgetPassword, useGenerateOTP } from '../hooks/auth-hooks';
import userService from '../service/user-service';
import { useNavigate } from 'react-router';
import { useDispatch } from 'react-redux';
import { setToken } from '../redux/reducer';
import Input from './ui/Input';
import { FaUser, FaLock } from 'react-icons/fa6';
import { BsFillEyeSlashFill, BsFillEyeFill } from 'react-icons/bs';
import Button from './ui/Button';
import Checkbox from './ui/Checkbox';
import MailIcon from './menu/icons/mailIcon';
import KeyIcon from './menu/icons/keyIcon';

const Login = () => {
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const errorObject: any = {};
  const valueObject: any = {
    email: '',
    password: '',
    is_remember_me: false,
  };
  const { mutate: loginData, isLoading } = useLoginAuth();
  const [values, setValues] = React.useState(valueObject);
  const [errors, setErrors] = React.useState(errorObject);
  const [passwordShown, setPasswordShown] = useState(false);
  const [message, setMessage] = React.useState('');
  const [rememberMe, setRememberMe] = useState(valueObject?.is_remember_me);
  const [checked, setChecked] = React.useState(false);
  const { mutate: passwordInstance } = useForgetPassword();
  const { mutate: otpInstance } = useGenerateOTP();
  interface CustomError extends Error {
    inner?: { path: string; message: string }[];
  }
  const handleChange = (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    const fieldName = event.target.name;
    setValues({ ...values, [fieldName]: event.target.value });
    setErrors({ ...errors, [fieldName]: '' });
  };
  const handleCheckbox = (event: React.ChangeEvent<HTMLInputElement>) => {
    setRememberMe(event.target.checked);
    const CheckboxValue = event.target.checked;
    setValues({ ...values, [event.target.name]: CheckboxValue });
  };
  const togglePasswordVisibility = () => {
    setPasswordShown(!passwordShown);
  };
  const handleSubmit = async (event: React.FormEvent) => {
    setMessage('');
    const schema = getLoginYupSchema(yup);
    await schema
      .validate(values, { abortEarly: false })
      .then(async () => {
        setErrors({});
        const data: any = {
          email_id: values?.email,
          user_password: values?.password,
          is_remember_me: rememberMe,
        };

        loginData(data, {
          onSuccess: async (data) => {
            if (data?.status === true) {
              dispatch(setToken({ key: 'Data', value: data }));
              const userData = await userService.getOneUser(values?.email);
              if (userData?.data?.is_initial_login === true) {
                const object: any = {
                  email_id: values?.email,
                };
                passwordInstance(object, {
                  onSuccess: (data, variables, context) => {
                    if (data?.status === true) {
                      window.location.href = data?.link;
                    }
                  },
                });
              } else if (
                !userData?.data?.is_initial_login &&
                userData?.data?.is_two_factor
              ) {
                const generateOtpObject = {
                  email_id: values?.email,
                };
                otpInstance(generateOtpObject);
                navigate('/generate-otp', { state: { email: values.email } });
              } else {
                navigate('/home');
              }
            } else setMessage('Username & Password is Incorrect');
          },
        });
      })
      .catch((e: CustomError) => {
        const errorObj: { [key: string]: string } = {};
        if (e.inner) {
          e.inner.map((error) => {
            return (errorObj[error.path] = error.message);
          });
        }
        setErrors({
          ...errorObj,
        });
      });
  };
  return (
    <div>
        <div className={Styles.container}>
          <div className={Styles.contentDiv}>
            <div className={Styles.titlediv}>
              <div className={Styles.main_title}></div>
              {/* <div className={Styles.logoCotainer}> */}
                <p className={Styles.sub_tile_words}>Omni <b> ERP</b></p>
              {/* </div> */}
            </div>
            <div className={Styles.filedContainer}>
              <div className={Styles.fields}>
                <Input
                  label="Username"
                  placeholder="Enter registered email"
                  name="email"
                  value={values.email}
                  onChange={(e) => handleChange(e)}
                  error={errors.email}
                  prefixIcon={<MailIcon style={{paddingTop: '5px'}}/>}
                  width="100%"
                />
                <Input
                  label="Password"
                  placeholder="Enter password"
                  name="password"
                  type={passwordShown ? 'text' : 'password'}
                  value={values.password}
                  onChange={handleChange}
                  error={errors.password}
                  prefixIcon={<KeyIcon />}
                  suffixIcon={
                    <button
                      type="button"
                      onClick={togglePasswordVisibility}
                      className={Styles.passToggle}
                    >
                      {passwordShown ? (
                        <BsFillEyeFill size={20} />
                      ) : (
                        <BsFillEyeSlashFill size={20} />
                      )}
                    </button>
                  }
                  width="100%"
                />
                <div className={Styles.errormessage}>
                  <span>{message}</span>
                </div>

                {/* <div className={Styles.buttonField}> */}
                  {/* <div className={Styles.forgetPassword}>
                    <Checkbox
                      name="is_remember_me"
                      checked={checked}
                      onChange={() => setChecked(!checked)}
                      label="Remember me"
                    />
                  </div> */}
                  <div className={Styles.forgetPassword}>
                    <a href="/forget-password">
                      <span>Forgot Password</span>
                    </a>
                  </div>
                {/* </div> */}
                <div className={Styles.buttons}>
                  <div className={Styles.loginButton}>
                    <Button
                      color="primary"
                      shape="rectangle"
                      justify="center"
                      size="small"
                      fullWidth
                      onClick={(e) => handleSubmit(e)}
                    >
                      Sign In
                    </Button>
                    <div className={Styles.copyRights}>
                      <p>Copyright 2023, Aalamsoft. All rights reserved.</p>
                    </div>
                  </div>
                  {/* <div className={Styles.newAccounts}>
                    <p className={Styles.newAccounts_msg}>
                      Don't have any account? <a href="#">Sign Up</a>
                    </p>
                  </div> */}
                </div>
              </div>
            </div>
          </div>
          <div className={Styles.imgContainer}>
            {/* <div className={Styles.imagediv}> */}
              <img src='/ERP.jpg' alt="erp" className={Styles.imagediv}/>
            {/* </div> */}
          </div>
        </div>
    </div>
  );
};

export default Login;
