import * as userDao from '../dao/user.dao';
import jwt from 'jsonwebtoken';
import md5 from 'md5';

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
    } = body;
    result = await userDao.add(
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
      updatedby
    );

    console.log('result', result);

    return result;
  } catch (error) {
    console.log('Error occurred in user service Add: ', error);
    if (
      // eslint-disable-next-line no-ex-assign, no-constant-condition
      (error = `duplicate key value violates unique constraint "users_username_unique"`)
    ) {
      return (result = { message: 'username already exists' });
    }
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
    result = await userDao.getById(userId);
    return result;

    /*  if (result === '[]') {
      return (result = { message: 'User Id Not Found' });
    } else {
      return result;
    } */
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
    result = await userDao.getByEmailId(emailId);
    return result;
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

    if (!existingUser || existingUser[0]?.userpass != md5(userpass)) {
      return (result = { message: 'Email Id and Password Not Matching' });
    }

    try {
      token = jwt.sign(
        { userId: existingUser[0].id, email: existingUser[0].email },
        'secretkeyappearshere',
        { expiresIn: '2h' }
      );
    } catch (err) {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      return (result = { message: 'Error! Something went wrong' });
    }

    const loginCredentials = {
      success: true,
      data: {
        userId: existingUser[0].id,
        email: existingUser[0].email,
        password: existingUser[0].userpass,
        username: existingUser[0].username,
        token: token,
      },
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
