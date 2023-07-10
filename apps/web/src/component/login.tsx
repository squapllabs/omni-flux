import React, { useState, ChangeEvent } from 'react';
import Styles from '../styles/login.module.scss';
import { IconButton, InputAdornment, Button, Checkbox } from '@mui/material';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOff from '@mui/icons-material/VisibilityOff';
import Person2Icon from '@mui/icons-material/Person2';
import LockIcon from '@mui/icons-material/Lock';
import GoogleIcon from '@mui/icons-material/Google';
import * as yup from 'yup';
import { getLoginYupSchema } from '../helper/constants/user-constants';
import { loginAuth } from '../hooks/auth-hooks';
import { useNavigate } from 'react-router';
import CircularProgress from '@mui/material/CircularProgress';
import { encryptPassword } from '../helper/password-handler';
import { useDispatch } from 'react-redux';
import { setToken } from '../redux/reducer';
import Customs from './ui/custom';
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
  const handleMouseDownnewPassword = (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    e.preventDefault();
  };
  const togglenewPassword = () => {
    setPasswordShown(!passwordShown);
  };
  const handleSubmit = async (event: React.FormEvent) => {
    setMessage('');
    const schema = getLoginYupSchema(yup);
    await schema
      .validate(values, { abortEarly: false })
      .then(async () => {
        setErrors({});
        const encryptPass = await encryptPassword(values?.password);
        const data: any = {
          email_id: values?.email,
          user_password: encryptPass,
          is_remember_me: rememberMe,
        };

        loginData(data, {
          onSuccess: (data, variables, context) => {
            if (data?.success === true) {
              dispatch(setToken({ key: 'Data', value: data }));
              navigate('/home');
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
              <div className={Styles.main_title}>
                {/* <span className={Styles.main_tile_words}>ENTERPRISE APPLICATION</span> */}
              </div>
              <div className={Styles.logoCotainer}>
                <img src="/loginImage.png" alt="aa" width="100" height="100" />
                <p className={Styles.sub_tile_words}>Enterprise Application</p>
              </div>
            </div>
            <div className={Styles.filedContainer}>
              <div className={Styles.fields}>
                <Customs.CustomTextField
                  size="small"
                  name="email"
                  label="Username"
                  variant="outlined"
                  fullWidth
                  InputProps={{
                    startAdornment: (
                      <InputAdornment position="start">
                        <Person2Icon />
                      </InputAdornment>
                    ),
                  }}
                  onChange={(e) => handleChange(e)}
                  error={errors.email}
                  helperText={errors.email}
                />
                <Customs.CustomTextField
                  size="small"
                  name="password"
                  label="Password"
                  type={passwordShown ? 'text' : 'password'}
                  variant="outlined"
                  fullWidth
                  InputProps={{
                    startAdornment: (
                      <InputAdornment position="start">
                        <LockIcon />
                      </InputAdornment>
                    ),
                    endAdornment: (
                      <InputAdornment position="end">
                        <IconButton
                          onMouseDown={(e) => handleMouseDownnewPassword(e)}
                        >
                          {passwordShown ? (
                            <VisibilityOff onClick={togglenewPassword} />
                          ) : (
                            <VisibilityIcon
                              onClick={togglenewPassword}
                              style={{ color: '#BEBFC5' }}
                            />
                          )}
                        </IconButton>
                      </InputAdornment>
                    ),
                  }}
                  onChange={(e) => handleChange(e)}
                  error={errors.password}
                  helperText={errors.password}
                />
                <div>
                  <span className={Styles.errormessage}>{message}</span>
                </div>
                <div className={Styles.buttonField}>
                  <div className={Styles.forgetPassword}>
                    <Checkbox
                      value={rememberMe}
                      onChange={(e) => handleCheckbox(e)}
                      size="small"
                    />{' '}
                    <span>Remember Me</span>
                  </div>
                  <div className={Styles.forgetPassword}>
                    <a href="/forget-password">
                      <span>Forgot Password ?</span>
                    </a>
                  </div>
                </div>
              </div>
              <div>
                <div className={Styles.loginButton}>
                  <Button
                    variant="contained"
                    color="primary"
                    onClick={(e) => handleSubmit(e)}
                    endIcon={
                      isLoading && (
                        <CircularProgress size={20} sx={{ color: 'white' }} />
                      )
                    }
                  >
                    Login
                  </Button>
                </div>
                <div className={Styles.divider}>OR</div>
                <div className={Styles.ssoButtons}>
                  <Button
                    variant="outlined"
                    color="primary"
                    className={Styles.iconColor}
                    startIcon={<GoogleIcon />}
                  >
                    Gmail
                  </Button>
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
