import React from 'react'
import Styles from '../styles/login.module.scss'
import image from '../assets/loginImage.png'
import logo from '../assets/logo.png'
import { TextField, InputAdornment, Button, Checkbox } from '@mui/material'
import Person2Icon from '@mui/icons-material/Person2';
import LockIcon from '@mui/icons-material/Lock';
import GoogleIcon from '@mui/icons-material/Google';
import * as yup from "yup";
import { getLoginYupSchema } from '../helper/constants/user-constants';
import { loginAuth } from '../hooks/user-hooks';
import { useNavigate } from 'react-router';
import CircularProgress from '@mui/material/CircularProgress';
import { setCookie } from '../helper/session';
import { encryptPassword } from '../helper/password-handler';
const Login = () => {
  const navigate = useNavigate();
  const errorObject: any = {}
  const valueObject: any = {
    email: "",
    password: ""
  }
  const { mutate: loginData, data: getToken, isLoading } = loginAuth();
  const [values, setValues] = React.useState(valueObject);
  const [errors, setErrors] = React.useState(errorObject);
  const [message, setMessage] = React.useState("");
  const handleChange = (event: any) => {
    setValues({ ...values, [event.target.name]: event.target.value })
  }
  const handleSubmit = async (event: any) => {
    const schema = getLoginYupSchema(yup)
    await schema
      .validate(values, { abortEarly: false })
      .then(async () => {
        setErrors({})
        const encryptPass = await encryptPassword(values?.password);
        console.log("encryptPass",encryptPass);
        
        const data: any = {
          email_id: values?.email,
          user_password: encryptPass
        }
        loginData(data, {
          onSuccess: (data, variables, context) => {
            if (data?.success === true) {
              navigate('/home');
              setCookie("logintoken", data?.token, 2);
            }
            else
              setMessage("Username & Password incorrect")
          }
        });
      })
      .catch((e: any) => {
        let errorObj: any = {};
        e.inner?.map((error: any) => {
          return (errorObj[error.path] = error.message);
        });
        setErrors({
          ...errorObj,
        });
      });
  }

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
                <img src='/loginImage.png' alt="aa" width="100" height="100" />
                <p className={Styles.sub_tile_words}>Enterprise Application</p>
              </div>
            </div>
            <div className={Styles.filedContainer}>
              <div className={Styles.fields}>
                <TextField
                  size='small'
                  name='email'
                  label='Username'
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
                <TextField
                  size='small'
                  name='password'
                  label='Password'
                  type='password'
                  InputProps={{
                    startAdornment: (
                      <InputAdornment position="start">
                        <LockIcon />
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
              </div>
              <div className={Styles.buttonField}>
                <div className={Styles.forgetPassword}>
                  <Checkbox defaultChecked size="small" />  <span>Remember me</span>
                </div>
                <div className={Styles.forgetPassword}>
                  <a href='#'><span>Forget Password ?</span></a>
                </div>
              </div>
              <div>
                <div className={Styles.loginButton}>
                  <Button variant='contained' color='primary' onClick={(e) => handleSubmit(e)} endIcon={isLoading && <CircularProgress size={20} sx={{ color: 'white' }} />}>Login</Button>
                </div>
                <div className={Styles.divider}>
                  OR
                </div>
                <div className={Styles.ssoButtons}>
                  <Button variant='outlined' color='primary' className={Styles.iconColor} startIcon={<GoogleIcon />}>Gmail</Button>
                </div>
              </div>

            </div>
            <div className={Styles.footer}>
            </div>
          </div>
          <div className={Styles.imagediv}>
          </div>
        </div>
      </div>
    </div>
  )
}

export default Login