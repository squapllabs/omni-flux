import React, { useState, ChangeEvent } from 'react';
import Styles from '../styles/login.module.scss';
import { IconButton, InputAdornment, Button, Checkbox } from '@mui/material';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOff from '@mui/icons-material/VisibilityOff';



import * as yup from 'yup';
import { getLoginYupSchema } from '../helper/constants/user-constants';
import { loginAuth, forgetPassword } from '../hooks/auth-hooks';
import userService from '../service/user-service';
import { useNavigate } from 'react-router';
import CircularProgress from '@mui/material/CircularProgress';
import { useDispatch } from 'react-redux';
import { setToken } from '../redux/reducer';
import Customs from './ui/custom';
import Input from './ui/Input';

import { FaUser, FaLock } from 'react-icons/fa6';
import { BsFillEyeSlashFill, BsFillEyeFill } from 'react-icons/bs';


interface Props {
  setIsAuth: React.Dispatch<React.SetStateAction<boolean>>;
}
const Login: React.FC<Props> = ({ setIsAuth }) => {
  const navigate = useNavigate();
  const dispatch = useDispatch();
  const errorObject: any = {};
  const valueObject: any = {
    email: '',
    password: '',
    is_remember_me: false,
  };
  const { mutate: loginData, isLoading } = loginAuth();
  const [values, setValues] = React.useState(valueObject);
  const [errors, setErrors] = React.useState(errorObject);
  const [passwordShown, setPasswordShown] = useState(false);
  const [message, setMessage] = React.useState('');
  const [rememberMe, setRememberMe] = useState(valueObject?.is_remember_me);
  const { mutate: passwordInstance } = forgetPassword();
  interface CustomError extends Error {
    inner?: { path: string; message: string }[];
  }

  const handleChange = (
    event: ChangeEvent<HTMLInputElement | HTMLTextAreaElement>
  ) => {
    setValues({ ...values, [event.target.name]: event.target.value });
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
          onSuccess: async (data, variables, context) => {
            if (data?.status === true) {
              dispatch(setToken({ key: 'Data', value: data }));
              navigate('/home');
              const userData = await userService.getOneUser(values?.email);
              if (userData?.data?.userData?.is_initial_login) {
                const object: any = {
                  email_id: values?.email,
                };
                passwordInstance(object, {
                  onSuccess: (data, variables, context) => {
                    if (data?.success === true) {
                      window.location.href = data?.link;
                    }
                  },
                });
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
      <div>
        <div className={Styles.container}>
          <div className={Styles.contantdiv}>
            <div className={Styles.titlediv}>
              <div className={Styles.main_title}></div>
              <div className={Styles.logoCotainer}>
                <img src="/Logomark.png" alt="aa" width="48px" height="48px" />
                <p className={Styles.sub_tile_words}>Welcome back</p>
                <p className={Styles.sub_title_sub}>
                  Welcome back! Please enter your details.
                </p>
              </div>
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
                  prefixIcon={<FaUser />}
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
                  prefixIcon={<FaLock />}
                  suffixIcon={
                    <button
                      type="button"
                      onClick={togglePasswordVisibility}
                      style={{ background: 'none', border: 'none' }}
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

                <div className={Styles.buttonField}>
                  <div className={Styles.forgetPassword}>
                    <Checkbox
                      value={rememberMe}
                      onChange={(e) => handleCheckbox(e)}
                      size="small"
                    />{' '}
                    <span>Remember me for 30 days</span>
                  </div>
                  <div className={Styles.forgetPassword}>
                    <a href="/forget-password">
                      <span>Forgot Password</span>
                    </a>
                  </div>
                </div>
                <div className={Styles.buttons}>
                  <div className={Styles.loginButton}>
                    <Button
                      variant="contained"
                      sx={{ backgroundColor: '#7f56d9' }}
                      onClick={(e) => handleSubmit(e)}
                      fullWidth
                      // className={classes.button}
                      endIcon={
                        isLoading && (
                          <CircularProgress size={20} sx={{ color: 'white' }} />
                        )
                      }
                    >
                      Login
                    </Button>
                  </div>

                  <div className={Styles.ssoButtons}>
                    <Button
                      variant="outlined"
                      sx={{ border: ' 1px solid #D0D5DD', color: '#344054' }}
                      className={Styles.iconColor}
                      startIcon={<img src="/Social_icon.png" />}
                    >
                      Sign in with Google
                    </Button>
                  </div>
                  <div className={Styles.newAccounts}>
                    <p className={Styles.newAccounts_msg}>
                      don't have any account? <a href="#">Sign in</a>
                    </p>
                  </div>
                </div>
              </div>
            </div>
            <div className={Styles.footer}></div>
          </div>
          <div className={Styles.imagediv}></div>
        </div>
      </div>
    </div>
  );
};

export default Login;
