import userDao from '../dao/user.dao';
import userRoleDao from '../dao/userRole.dao';
import jwt from 'jsonwebtoken';
import md5 from 'md5';
import prisma from '../utils/prisma';
import { AES, enc } from 'crypto-js';
import { createUserBody, updateUserBody } from '../interfaces/userInterface';

/**
 * Method to Create a New User
 * @param body
 * @returns
 */
const createUser = async (body: createUserBody) => {
  let result = null;
  try {
    const {
      user_password,
      contact_no,
      email_id,
      first_name,
      last_name,
      user_status,
      address = null,
      created_by,
      updated_by = null,
      role_id,
    } = body;

    const userEmailExist = await userDao.getByEmailId(email_id);
    if (userEmailExist) {
      return (result = { success: false, message: 'email id already exists' });
    }

    const userDataWithRole = [];
    result = await prisma
      .$transaction(async (prisma) => {
        const userDetails = await userDao.add(
          md5(user_password),
          contact_no,
          email_id,
          first_name,
          last_name,
          user_status,
          address,
          created_by,
          updated_by,
          prisma
        );
        userDataWithRole.push({ userData: userDetails });

        if (userDetails) {
          const userRoleData = await userRoleDao.add(
            role_id,
            userDetails?.user_id,
            created_by,
            updated_by,
            prisma
          );
          userDataWithRole.push({ userRoleData: userRoleData });
        }
        return userDataWithRole;
      })
      .then((data) => {
        console.log('Successfully User Data Returned ', data);
        const newUserData = {
          success: true,
          data: data,
        };
        return newUserData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in user service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing User
 * @param body
 * @returns
 */
const updateUser = async (body: updateUserBody) => {
  let result = null;
  try {
    const {
      user_password,
      contact_no,
      email_id,
      first_name,
      last_name,
      user_status,
      address = null,
      created_by,
      updated_by = null,
      role_id,
      user_id,
    } = body;

    const userExist: { email_id: string } = await userDao.getById(user_id);
    if (!userExist) {
      return (result = { success: false, message: 'user does not exists' });
    }
    const userEmailExist = await userDao.getByEmailId(email_id);
    if (userEmailExist && !(email_id === userExist?.email_id)) {
      return (result = { success: false, message: 'email id already exists' });
    }

    const userDataWithRole = [];
    result = await prisma
      .$transaction(async (prisma) => {
        const userDetails = await userDao.edit(
          md5(user_password),
          contact_no,
          email_id,
          first_name,
          last_name,
          user_status,
          address,
          created_by,
          updated_by,
          user_id,
          prisma
        );
        userDataWithRole.push({ userData: userDetails });

        if (userDetails) {
          const userRoleData = await userRoleDao.edit(
            role_id,
            userDetails?.user_id,
            created_by,
            updated_by,
            prisma
          );
          userDataWithRole.push({ userRoleData: userRoleData });
        }
        return userDataWithRole;
      })
      .then((data) => {
        console.log('Successfully User Data Returned ', data);
        const newUserData = {
          success: true,
          data: data,
        };
        return newUserData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in user service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get User By UserId
 * @param userId
 * @returns
 */
const getById = async (userId: bigint) => {
  try {
    let result = null;
    const userData = await userDao.getById(userId);
    if (userData) {
      result = { success: true, data: userData };
      return result;
    } else {
      result = { success: false, message: 'user id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById user service : ', error);
    throw error;
  }
};

/**
 * Method to get User By email
 * @param emailId
 * @returns
 */
const getByEmailId = async (emailId: string) => {
  try {
    let result = null;
    const userData = await userDao.getByEmailId(emailId);

    if (userData) {
      result = { success: true, data: userData };
      return result;
    } else {
      result = { success: false, message: 'user email not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByEmailId user service : ', error);
    throw error;
  }
};

/**
 * Method for User Login
 * @param body
 * @returns
 */
const userLogin = async (
  body: { email_id: string; user_password: string; is_remember_me: boolean },
  res
) => {
  try {
    let token: string;
    let result = null;
    const { email_id, user_password, is_remember_me } = body;
    const existingUser = await userDao.getByEmailId(email_id);
    if (!existingUser || existingUser?.user_password !== md5(user_password)) {
      result = {
        success: false,
        message: 'Email id and password Wrong',
      };
      return result;
    }

    try {
      token = jwt.sign(
        { userId: existingUser.user_id, email: existingUser.email_id },
        process.env.API_ACCESS_TOKEN_SECRET_KEY,
        { expiresIn: '2h' }
      );
    } catch (err) {
      console.log(' error occurred', err);
      return (result = {
        success: false,
        message: 'Error! Something went wrong',
      });
    }
    const fullName = existingUser?.first_name + ' ' + existingUser?.last_name;
    const loginCredentials = {
      success: true,
      token: `Bearer ${token}`,
      fullName: fullName,
    };

    /* Here the expiration period is set for 1 Day */

    const expirationDate = new Date();
    expirationDate.setDate(expirationDate.getDate() + 1);

    const cookieOptions = {
      expires: is_remember_me === true ? expirationDate : null,
      secure: true,
      httpOnly: false,
      sameSite: 'None',
    };

    res
      .cookie('Token', `Bearer ${token}`, cookieOptions)
      .cookie('Name', fullName, cookieOptions)
      .send(loginCredentials);
  } catch (error) {
    console.log('Error occurred in userLogin user service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Users
 * @returns
 */
const getAllUser = async (user_status = 'AC') => {
  try {
    const result = await userDao.getAll(user_status);
    const userData = { success: true, data: result };
    return userData;
  } catch (error) {
    console.log('Error occurred in getAll user service : ', error);
    throw error;
  }
};

/**
 * Method for User Logout
 * @param req
 * @param res
 */
const userLogOut = (req, res) => {
  try {
    res.clearCookie('Token');
    res.clearCookie('Name');
    res.json({ success: true, message: 'LogOut successful' });
  } catch (error) {
    console.log('Error occurred in userLogout: ', error);
    res
      .status(500)
      .json({ success: false, message: 'Error occurred during logOut' });
  }
};

/**
 * Method to delete user
 * @param userId
 */
const deleteUser = async (userId) => {
  try {
    const userExist = await userDao.getById(userId);

    if (!userExist) {
      const result = { success: false, message: 'User Id Not Exist' };
      return result;
    }
    const data = await userDao.deleteUser(userId);
    if (data?.is_delete === true) {
      const result = { success: true, message: 'Data Deleted Successfully' };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this user' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteUser user service : ', error);
    throw error;
  }
};

export {
  createUser,
  updateUser,
  getById,
  getByEmailId,
  userLogin,
  getAllUser,
  userLogOut,
  deleteUser,
};
