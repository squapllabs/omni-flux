import React from 'react';
import { Button, Card, TextField } from '@mui/material';
import Styles from '../styles/fortgetPassword.module.scss';
import LockPersonIcon from '@mui/icons-material/LockPerson';
import { forgetPassword } from '../hooks/auth-hooks';
const ForgetPassword = () => {
  const valueObject: any = {
    email: '',
  };
  const [values, setValues] = React.useState(valueObject);
  const {
    mutate: passwordInstance,
    data: getToken,
    isLoading,
  } = forgetPassword();
  const handleChange = (event: any) => {
    setValues({ ...values, [event.target.name]: event.target.value });
  };
  const handleSubmit = () => {
    const data: any = {
      email_id: values?.email,
    };
    passwordInstance(data, {
      onSuccess: (data, variables, context) => {
        console.log('data', data);
      },
    });
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
              <TextField
                name="email"
                type="email"
                label="Email"
                size="small"
                sx={{ width: '320px' }}
                onChange={handleChange}
              />
            </div>
            <div>
              <Button onClick={handleSubmit} variant="outlined">
                send
              </Button>
            </div>
          </Card>
        </div>
        <div className={Styles.footer}></div>
      </div>
    </div>
  );
};

export default ForgetPassword;
