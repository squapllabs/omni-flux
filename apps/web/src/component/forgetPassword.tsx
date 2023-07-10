import React from 'react';
import { Card } from '@mui/material';
import Styles from '../styles/fortgetPassword.module.scss';
import LockPersonIcon from '@mui/icons-material/LockPerson';
import { forgetPassword } from '../hooks/auth-hooks';
import MySnackbar from './ui/MySnackbar';
import Customs from './ui/custom';
import { useNavigate } from 'react-router';
interface ValueObject {
  email: string;
}
const ForgetPassword = () => {
  const navigate = useNavigate();
  const valueObject: ValueObject = {
    email: '',
  };
  const [values, setValues] = React.useState(valueObject);
  const [message, setMessage] = React.useState('');
  const [isWarning, setIswarning] = React.useState(false);
  const [open, setOpen] = React.useState(false);
  const { mutate: passwordInstance } = forgetPassword();
  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setValues({ ...values, [event.target.name]: event.target.value });
  };
  const handleSubmit = () => {
    const data: any = {
      email_id: values?.email,
    };
    passwordInstance(data, {
      onSuccess: (data, variables, context) => {
        if (data?.success === true) {
          setIswarning(false);
          setMessage('Reset link shared to your account');
          handleClick();
          setInterval(() => {
            navigate('/');
          }, 3000);
        } else {
          setIswarning(true);
          setMessage(data?.message);
          handleClick();
        }
      },
    });
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
  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.header}></div>
        <div className={Styles.main}>
          <Card className={Styles.mainCard}>
            <div className={Styles.content_card}>
              <div>
                <LockPersonIcon sx={{ opacity: 0.5 }} />
              </div>
              <div className={Styles.content}>
                <span>
                  Enter your email address and we'll send you a link to reset
                  your password
                </span>
              </div>
            </div>
            <div>
              <Customs.CustomTextField
                name="email"
                type="email"
                label="Email"
                size="small"
                variant="outlined"
                fullWidth
                onChange={handleChange}
              />
            </div>
            <div>
              <Customs.CustomButton
                onClick={handleSubmit}
                variant="outlined"
                label="send"
              />
            </div>
          </Card>
        </div>
        <div className={Styles.footer}></div>
      </div>
      <MySnackbar
        open={open}
        message={message}
        onClose={handleClose}
        severity={isWarning === true ? 'error' : 'success'}
        autoHideDuration={3000}
      />
    </div>
  );
};

export default ForgetPassword;
