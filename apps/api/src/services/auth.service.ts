import authDao from '../dao/auth.dao';
import userDao from '../dao/user.dao';
import jwt from 'jsonwebtoken';
import md5 from 'md5';
import otpGenerator from '../utils/otpGenerator';
import mailService from './mail.service';
import { userExistBody } from '../interfaces/user.Interface';

/**
 * Method for forget password url generate
 * @param body
 * @returns
 */
const forgetPassword = async (body: { email_id: string }) => {
  try {
    let result = null;
    const { email_id } = body;
    const userCheckExist = await userDao.getByEmailId(email_id);
    if (userCheckExist) {
      const userId = userCheckExist.user_id;
      const token = jwt.sign(
        { userId, email_id },
        process.env.NEXT_PUBLIC_FORGETPASSWORD_TOKEN_SECRET_KEY,
        { expiresIn: '2h' }
      );
      const link = `${process.env.REACT_APP_URL}/reset-password/${userId}/${token}/`;
      console.log('link ==>', link);
      result = { success: true, link: link };
      return result;
    } else {
      result = { success: false, message: 'user email not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in forget password : ', error);
    throw error;
  }
};

/**
 * Method for Reset Password
 * @param body
 * @returns
 */
const updatePassword = async (body: {
  email_id: string;
  user_password: string;
}) => {
  try {
    let result = null;
    const { email_id, user_password } = body;
    const userCheckExist = await userDao.getByEmailId(email_id);

    if (userCheckExist) {
      result = await authDao.editPassword(email_id, md5(user_password));
      return (result = { success: true, data: result });
    } else {
      return (result = { success: false, message: 'email_id does not exist' });
    }
  } catch (error) {
    console.log('Error occurred in update password : ', error);
    throw error;
  }
};

/**
 * Method to generate OTP
 * @param body
 * @returns
 */
const generateOTP = async (body: { email_id: string }) => {
  try {
    let result = null;
    const { email_id } = body;
    const userCheckExist = await userDao.getByEmailId(email_id);
    if (userCheckExist) {
      const otpData = otpGenerator.generateOTP();
      const otpUpdateInUser = await userDao.updateOTP(
        otpData.otpSecret,
        0,
        otpData.otpExpirationDate,
        userCheckExist.user_id,
        false
      );

      const emailCredentials = {
        otp_secret: otpData.otpSecret,
        user_name: userCheckExist.first_name + ' ' + userCheckExist.last_name,
        to_email_id: email_id,
      };

      await mailService.OTPEmail(emailCredentials);

      result = {
        message: 'success',
        status: true,
        data: otpUpdateInUser,
      };
      return result;
    } else {
      result = {
        message: 'email_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in Auth Service generateOTP : ', error);
    throw error;
  }
};

/**
 * Method to verify OTP
 * @param body
 * @returns
 */
const verifyOTP = async (body: { email_id: string; otp_secret: number }) => {
  try {
    let result = null;
    const { email_id, otp_secret } = body;

    const userCheckExist = (await userDao.getByEmailId(
      email_id
    )) as userExistBody;

    if (userCheckExist) {
      const currentTimestamp = new Date();
      const attempt = userCheckExist.otp_attempts;

      const max_otp_attempt_allowed = Number(process.env.MAX_ATTEMPTS_FOR_OTP);

      if (attempt < max_otp_attempt_allowed) {
        await userDao.updateOTP(
          userCheckExist.otp_secret,
          attempt + 1,
          userCheckExist.otp_expired_in,
          userCheckExist.user_id,
          false
        );
        const isOTPValid = currentTimestamp <= userCheckExist.otp_expired_in;

        const otpVerification = otp_secret === userCheckExist.otp_secret;

        if (isOTPValid === true && otpVerification === true) {
          const otpVerifiedData = await userDao.updateOTP(
            null,
            attempt + 1,
            null,
            userCheckExist.user_id,
            true
          );

          result = {
            message: 'OTP Verified Successfully!',
            status: true,
            data: otpVerifiedData,
          };
          return result;
        } else if (isOTPValid === false) {
          result = {
            message: 'OTP Expired',
            status: false,
            data: null,
          };
          return result;
        } else if (otpVerification === false) {
          result = {
            message: 'Wrong OTP.Try Again!',
            status: false,
            data: null,
          };
          return result;
        }
      } else {
        result = {
          message: 'Maximum Attempt Reached',
          status: false,
          data: null,
        };
        return result;
      }
    } else {
      result = {
        message: 'email_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in Auth Service verifyOTP : ', error);
    throw error;
  }
};
export { forgetPassword, updatePassword, generateOTP, verifyOTP };
