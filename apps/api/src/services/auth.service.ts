import authDao from '../dao/auth.dao';
import userDao from '../dao/user.dao';
import jwt from 'jsonwebtoken';
import md5 from 'md5';

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
      result = { message: 'success', status: true, link: link };
      return result;
    } else {
      result = { message: 'user email not exist', status: false, data: null };
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
      return (result = { message: 'success', status: true, data: result });
    } else {
      return (result = { status: false, data: null, message: 'email_id does not exist' });
    }
  } catch (error) {
    console.log('Error occurred in update password : ', error);
    throw error;
  }
};

export { forgetPassword, updatePassword };
