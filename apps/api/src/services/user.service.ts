import db from '../utils/db';
/* eslint-disable @typescript-eslint/no-var-requires */
import userDao from '../dao/user.dao';
import userRoleDao from '../dao/user-role.dao';
const jwt = require('jsonwebtoken');
const md5 = require('md5');

/**
 * Method to Create a New User
 * @param body
 * @returns
 */
const createUser = async (body: {
  center_id: BigInteger;
  user_name: string;
  user_password: string;
  mobile_number: string;
  email_id: string;
  first_name: string;
  last_name: string;
  profile_img_url: string;
  gender: string;
  dob: Date;
  status: string;
  address: string;
  created_by: BigInteger;
  updated_by: BigInteger;
  role_id: BigInteger;
}) => {
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

    const userNameExist = await userDao.getByUserName(user_name);

    if (userNameExist) {
      return (result = { success: false, message: 'username already exists' });
    }

    result = await db
      .tx(async (transaction) => {
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
          transaction
        );

        if (userDetails) {
          // eslint-disable-next-line @typescript-eslint/no-unused-vars
          const userRoleData = await userRoleDao.add(
            role_id,
            userDetails?.user_id,
            transaction
          );
        }

        return userDetails;
      })
      .then((data: { user_id: bigint }) => {
        console.log('successfully data returned', data.user_id);
        const newUserData = {
          success: true,
          data: data,
        };
        return newUserData;
      })
      .catch((error: string) => {
        console.log('failure, ROLLBACK was executed', error);
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
      return (result = { success: true, data: userData });
    } else {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      return (result = { success: false, message: 'user id not exist' });
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
      return (result = { success: true, data: userData });
    } else {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      return (result = { success: false, message: 'user email not exist' });
    }
  } catch (error) {
    console.log('Error occurred in getByEmailId user service : ', error);
    throw error;
  }
};

const userLogin = async (body: { email_id: string; user_password: string }) => {
  try {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    let token: any;
    let result = null;
    const { email_id, user_password } = body;
    const existingUser = await userDao.getByEmailId(email_id);

    if (!existingUser) {
      return (result = {
        success: false,
        message: 'Email Id Not Found',
      });
    }

    if (existingUser?.user_password != md5(user_password)) {
      return (result = {
        success: false,
        message: 'Wrong Password',
      });
    }

    try {
      token = jwt.sign(
        { userId: existingUser.user_id, email: existingUser.email_id },
        process.env.API_ACCESS_TOKEN_SECRET_KEY,
        { expiresIn: '2h' }
      );
    } catch (err) {
      console.log(' error: err', err);

      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      return (result = {
        success: false,
        message: 'Error! Something went wrong',
      });
    }

    const loginCredentials = {
      success: true,
      token: token,
    };

    /* result = userDao.userLogin(email, userpass); */
    return loginCredentials;
  } catch (error) {
    console.log('Error occurred in userLogin user service : ', error);
    throw error;
  }
};

/**
 * Method to get all User
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
