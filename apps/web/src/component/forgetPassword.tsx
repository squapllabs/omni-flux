import React from 'react';
import Styles from '../styles/fortgetPassword.module.scss';
import { useForgetPassword } from '../hooks/auth-hooks';
import CustomSnackBar from './ui/customSnackBar';
import { useNavigate } from 'react-router';
import CustomCard from './ui/CustomCard';
import Input from './ui/Input';
import Button from './ui/Button';
import LockIcon from './menu/icons/lockicon';
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
  const { mutate: passwordInstance } = useForgetPassword();
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
          <CustomCard>
            <div className={Styles.content_card}>
              <div className={Styles.lockicon}>
                <LockIcon />
              </div>
              <div className={Styles.content}>
                <span>
                  Enter your email address and we'll send you a link to reset
                  your password
                </span>
              </div>
            </div>
            <div>
              <Input
                name="email"
                type="email"
                label="Email"
                onChange={handleChange}
              />
            </div>
            <div>
              <Button
                className={Styles.searchButton}
                onClick={handleSubmit}
                shape="rectangle"
                justify="center"
                size="small"
              >
                Send
              </Button>
            </div>
          </CustomCard>
        </div>
        <div className={Styles.footer}></div>
      </div>
      <CustomSnackBar
        open={open}
        message={message}
        onClose={handleClose}
        type={ isWarning === true ? "error": "success"}
        autoHideDuration={3000}
      />
    </div>
  );
};

export default ForgetPassword;
