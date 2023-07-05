import userDao from '../dao/user.dao';
import userRoleDao from '../dao/user-role.dao';
import jwt from 'jsonwebtoken';
import md5 from 'md5';
import prisma from '../utils/prisma';
import { AES, enc } from 'crypto-js';
import { CreateUserBody } from '../interfaces/user-interface';

/**
 * Method to Create a New User
 * @param body
 * @returns
 */
const createUser = async (body: CreateUserBody) => {
  let result = null;
  try {
    const {
      center_id = null,
      user_name = null,
      user_password = null,
      mobile_number = null,
      email_id,
      first_name = null,
      last_name = null,
      profile_img_url = null,
      gender = null,
      dob = null,
      status = null,
      address = null,
      created_by = null,
      updated_by = null,
      role_id,
    } = body;

    const userEmailExist = await userDao.getByEmailId(email_id);
    if (userEmailExist) {
      return (result = { success: false, message: 'email id already exists' });
    }

    if (user_name) {
      const userNameExist = await userDao.getByUserName(user_name);
      if (userNameExist) {
        return (result = {
          success: false,
          message: 'username already exists',
        });
      }
    }
    const userDataWithRole = [];
    result = await prisma
      .$transaction(async (prisma) => {
        const userDetails = await userDao.add(
          center_id,
          user_name,
          md5(user_password),
          mobile_number,
          email_id,
          first_name,
          last_name,
          profile_img_url,
          gender,
          dob,
          status,
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
    const decryptedPassword = AES.decrypt(
      user_password,
      process.env.AUTH_SECRET_KEY
    ).toString(enc.Utf8);

    if (
      !existingUser ||
      existingUser?.user_password !== md5(decryptedPassword)
    ) {
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
      token: token,
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
      .cookie('Token', token, cookieOptions)
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
const getAllUser = async () => {
  try {
    const result = await userDao.getAllUserData();
    const userData = { success: true, data: result };
    return userData;
  } catch (error) {
    console.log('Error occurred in getAll user service : ', error);
    throw error;
  }
};

export { createUser, getById, getByEmailId, userLogin, getAllUser };
