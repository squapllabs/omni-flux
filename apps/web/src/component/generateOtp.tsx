import React from 'react';
import Styles from '../styles/generateOtp.module.scss';
import CustomCard from './ui/CustomCard';
import Input from './ui/Input';
import Button from './ui/Button';
import { verifyOTP } from '../hooks/auth-hooks';
import * as yup from 'yup';
import { useNavigate } from 'react-router';
import { getValidateOTPYupSchema } from '../helper/constants/user-constants';
import { useLocation } from 'react-router-dom';

const GenerateOtp = (props: any) => {
  const [message, setMessage] = React.useState('');
  const location = useLocation();
  const email = location.state?.email;
  const { mutate: otpData } = verifyOTP();
  const navigate = useNavigate();
  const errorObject: any = {};
  const valueObject: any = {
    otp: '',
    email_id: email
  };
  const [values, setValues] = React.useState(valueObject);
  const [errors, setErrors] = React.useState(errorObject);
  interface CustomError extends Error {
    inner?: { path: string; message: string }[];
  }
  const handleChange = (event: any) => {
    setValues({ ...values, [event.target.name]: event.target.value });
  };
  const handleSubmit = async (event: React.FormEvent) => { 
    setMessage('');
    const schema = getValidateOTPYupSchema(yup);
    await schema
      .validate(values, { abortEarly: false })
      .then(async () => {
        setErrors({});
        const data: any = {
          email_id: email,
          otp_secret: parseInt(values?.otp, 10),
        };
        otpData(data, {
          onSuccess: (data) => {
            if (data) {
              if (data?.status === true) {
                navigate('/home');
              }
              else {
                setMessage('Invalid OTP Entered');
              }
            }
          },
        })
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
      })
  }
  return (
    <div>
      <div className={Styles.container}>
        <div className={Styles.header}></div>
        <div className={Styles.main}>
          <CustomCard>
            <div className={Styles.content}>
              <span>Please enter the One-Time Password to verify your account</span>
            </div>
            <br />
            <div className={Styles.content_card}>
              <div className={Styles.content}>
                <span>A One-Time Password has been sent to your E-Mail</span>
              </div>
            </div>
            <div className={Styles.containerStyle}>
              <Input
                placeholder="Enter One-Time Password"
                name="otp"
                onChange={(e) => handleChange(e)}
                value={values.otp}
                width="250px"
                error={errors.otp}
              />
            </div>
            <div className={Styles.errormessage}>
              <span>{message}</span>
            </div>
            <div className={Styles.buttonStyle}>
              <Button
                color="primary"
                shape="rectangle"
                justify="center"
                size="small"
                onClick={(e) => handleSubmit(e)}
              >
                verify
              </Button>
            </div>
          </CustomCard>
        </div>
      </div>
    </div>
  );
}
export default GenerateOtp;
