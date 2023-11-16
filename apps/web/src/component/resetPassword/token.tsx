import React, { useState } from 'react';
import { useParams } from 'react-router-dom';
import { Button, Card, TextField } from '@mui/material';
import Styles from '../../styles/fortgetPassword.module.scss';
import * as yup from 'yup';
import { getForgetPasswordYupSchema } from '../../helper/constants/user-constants';
import {getByuserID} from '../../hooks/user-hooks'
import {useResetPassword,useSetTwoFA} from '../../hooks/auth-hooks'
import CircularProgress from '@mui/material/CircularProgress';
import { useNavigate } from 'react-router';
import { IconButton, InputAdornment } from '@mui/material';
import VisibilityIcon from '@mui/icons-material/Visibility';
import VisibilityOff from '@mui/icons-material/VisibilityOff';
import CustomSnackbar from '../ui/customSnackBar';
const ResetPassword = () => {
  const routeParams = useParams();
  const navigate = useNavigate();
  const userId = Number(routeParams?.id);
  const { data: getuserData } = getByuserID(userId);
  const { mutate: restPassword, isLoading } = useResetPassword();
  const { mutate: setTwofa } = useSetTwoFA();
  const errorObject: any = {};
  const valueObject: any = {
    new_password: '',
    confirm_password: '',
    enableTwoFA: false,
  };
  const [values, setValues] = React.useState(valueObject);
  const [errors, setErrors] = React.useState(errorObject);
  const [message, setMessage] = React.useState('');
  const [waring, setwaring] = React.useState(false);
  const [open, setOpen] = React.useState(false);
  const [newPasswordShown, setNewPasswordShown] = React.useState(false);
  const [confirmPasswordShown, setConfirmPasswordShown] = React.useState(false);
  const [enableTwoFA, setEnableTwoFA] = useState(false);
  const handleTwoFAToggle = () => {
    setEnableTwoFA(!enableTwoFA);
    console.log(enableTwoFA);
  };
  const handleClick = () => {
    setOpen(true);
  };
  const handleClose = (
    event?: React.SyntheticEvent | Event,
    reason?: string
  ) => {
    if (reason === 'clickaway') {
      return;
    }
    setOpen(false);
  };
  const handleMouseDownnewPassword = (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    e.preventDefault();
  };
  const handleMouseDownconfirmPassword = (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    e.preventDefault();
  };
  const togglePassword = () => {
    setNewPasswordShown(!newPasswordShown);
  };
  const toggleconfirmPassword = () => {
    setConfirmPasswordShown(!confirmPasswordShown);
  };
  const handleChange = (event: any) => {
    const { name, type, value, checked } = event.target;
    if (type === 'checkbox') {
      setValues((prevValues: any) => ({
        ...prevValues,
        [name]: checked,
      }));
    } else {
      setValues((prevValues: any) => ({
        ...prevValues,
        [name]: value,
      }));
    }
  };
  interface CustomError extends Error {
    inner?: { path: string; message: string }[];
  }
  const handleSubmit = async () => {
    const schema = getForgetPasswordYupSchema(yup);
    await schema
      .validate(values, { abortEarly: false })
      .then(async () => {
        setErrors({});
        const data: any = {
          email_id: getuserData?.userData?.email_id,
          user_password: values?.confirm_password,
        };
        restPassword(data, {
          onSuccess: (data, variables, context) => {
            if (data?.status === true) {
              setwaring(false);
              setMessage('Password has reseted successfully');
              handleClick();
              const data: any = {
                user_id: getuserData?.userData?.user_id,
                is_two_factor: enableTwoFA,
              };
              setTwofa(data, {
                onSuccess: (data, variables, context) => {
                  if (data?.is_two_factor === true) {
                    setMessage('Two factor Authentication enabled');
                  }
                },
              });
              setInterval(() => {
                navigate('/');
              }, 3000);
            } else {
              setwaring(true);
              setMessage('Try again');
            }
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
        <div className={Styles.header}></div>
        <div className={Styles.main}>
          <Card className={Styles.mainCard}>
            <div className={Styles.content_card}>
              <div className={Styles.password_content}>
                <span>Reset account Password</span>
              </div>
            </div>
            <div className={Styles.password_fields}>
              <TextField
                name="new_password"
                label="New Password"
                size="small"
                type={newPasswordShown ? 'text' : 'password'}
                value={values?.new_password}
                fullWidth
                onChange={handleChange}
                error={errors.new_password}
                helperText={errors.new_password}
                InputProps={{
                  endAdornment: (
                    <InputAdornment position="end">
                      <IconButton
                        onMouseDown={(e) => handleMouseDownnewPassword(e)}
                      >
                        {newPasswordShown ? (
                          <VisibilityIcon onClick={togglePassword} />
                        ) : (
                          <VisibilityOff
                            onClick={togglePassword}
                            className={Styles.colour}
                            
                          />
                        )}
                      </IconButton>
                    </InputAdornment>
                  ),
                }}
              />
              <TextField
                name="confirm_password"
                type={confirmPasswordShown ? 'text' : 'password'}
                label="Confirm Password"
                size="small"
                value={values?.confirm_password}
                fullWidth
                onChange={handleChange}
                error={errors.confirm_password}
                helperText={errors.confirm_password}
                InputProps={{
                  endAdornment: (
                    <InputAdornment position="end">
                      <IconButton
                        onMouseDown={(e) => handleMouseDownconfirmPassword(e)}
                      >
                        {confirmPasswordShown ? (
                          <VisibilityIcon onClick={toggleconfirmPassword} />
                        ) : (
                          <VisibilityOff
                            onClick={toggleconfirmPassword}
                            className={Styles.colour}
                          />
                        )}
                      </IconButton>
                    </InputAdornment>
                  ),
                }}
              />
            </div>
            <div>
              <span>Enable 2-factor authentication</span>
              <label className="switch">
                <input
                  type="checkbox"
                  checked={enableTwoFA}
                  onChange={handleTwoFAToggle}
                />
              </label>
            </div>
            <div>
              <Button
                onClick={handleSubmit}
                variant="contained"
                endIcon={
                  isLoading && (
                    <CircularProgress size={20} sx={{ color: 'white' }} />
                  )
                }
              >
                Submit
              </Button>
            </div>
          </Card>
        </div>
        <div className={Styles.footer}></div>
      </div>
      <CustomSnackbar
        open={open}
        message={message}
        onClose={handleClose}
        autoHideDuration={3000}
        type={waring === false ? 'success' : 'error'}
      />
    </div>
  );
};
export default ResetPassword;
