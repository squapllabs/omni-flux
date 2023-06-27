import db from '../utils/db';

/* eslint-disable @typescript-eslint/no-var-requires */
const userDao = require('../dao/user.dao');
const userRoleDao = require('../dao/user-role.dao');
const jwt = require('jsonwebtoken');
const md5 = require('md5');

/**
 * Method to Create a New User
 * @param body
 * @returns
 */
const createUser = async (body: {
  center_id: BigInteger;
  username: string;
  userpass: string;
  mobilenumber: string;
  email: string;
  firstname: string;
  lastname: string;
  profileimgurl: string;
  gender: string;
  dob: Date;
  status: string;
  address: string;
  createdby: BigInteger;
  updatedby: BigInteger;
  role_id: BigInteger;
}) => {
  let result = null;
  try {
    const {
      center_id,
      username,
      userpass,
      mobilenumber,
      email,
      firstname,
      lastname,
      profileimgurl,
      gender,
      dob,
      status,
      address,
      createdby,
      updatedby,
      role_id,
    } = body;

    const userEmailExist = await userDao.getByEmailId(email);

    if (userEmailExist) {
      return (result = { success: false, message: 'email id already exists' });
    }

    const userNameExist = await userDao.getByUserName(username);

    if (userNameExist) {
      return (result = { success: false, message: 'username already exists' });
    }

    result = await db
      .tx(async (transaction) => {
        const userDetails = await userDao.add(
          center_id,
          username,
          md5(userpass),
          mobilenumber,
          email,
          firstname,
          lastname,
          profileimgurl,
          gender,
          dob,
          status,
          address,
          createdby,
          updatedby,
          transaction
        );

        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        const userRoleData = await userRoleDao.add(
          role_id,
          userDetails?.id,
          transaction
        );

        return userDetails;
      })
      .then((data: { id: bigint }) => {
        console.log('successfully data returned', data.id);
        return data;
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

const userLogin = async (body: { email: string; userpass: string }) => {
  try {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    let token: any;
    let result = null;
    const { email, userpass } = body;
    const existingUser = await userDao.getByEmailId(email);

    if (!existingUser) {
      return (result = {
        success: false,
        message: 'Email Id Not Found',
      });
    }

    if (existingUser?.userpass != md5(userpass)) {
      return (result = {
        success: false,
        message: 'Wrong Password',
      });
    }

    try {
      token = jwt.sign(
        { userId: existingUser.id, email: existingUser.email },
        'secretkeyappearshere',
        { expiresIn: '2h' }
      );
    } catch (err) {
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
    return result;
  } catch (error) {
    console.log('Error occurred in getAll user service : ', error);
    throw error;
  }
};

export { createUser, getById, getByEmailId, userLogin, getAllUser };
