import userDao from '../dao/user.dao';
import userRoleDao from '../dao/userRole.dao';
import jwt, { JwtPayload } from 'jsonwebtoken';
import md5 from 'md5';
import prisma from '../utils/prisma';
import { createUserBody, updateUserBody } from '../interfaces/user.Interface';
import customQueryExecutor from '../dao/common/utils.dao';

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
      user_status = 'AC',
      address = null,
      created_by,
      role_id,
      department,
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
          department,
          prisma
        );
        userDataWithRole.push({ userData: userDetails });

        if (userDetails) {
          const userRoleData = await userRoleDao.add(
            role_id,
            userDetails?.user_id,
            created_by,
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
      first_name,
      last_name,
      address,
      updated_by,
      role_id,
      department,
      user_id,
    } = body;

    const userExist = await userDao.getById(user_id);
    if (!userExist) {
      return (result = { success: false, message: 'user id does not exists' });
    }

    const existingUserRoleData = await userRoleDao.getByUserId(user_id);

    const userDataWithRole = [];
    result = await prisma
      .$transaction(async (prisma) => {
        const userDetails = await userDao.edit(
          first_name,
          last_name,
          address,
          updated_by,
          user_id,
          department,
          prisma
        );
        userDataWithRole.push({ userData: userDetails });

        if (userDetails) {
          const userRoleData = await userRoleDao.edit(
            role_id,
            userDetails?.user_id,
            updated_by,
            Number(existingUserRoleData?.user_role_id),
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
const getById = async (userId: number) => {
  try {
    let result = null;
    const userData = await userDao.getById(userId);
    if (userData) {
      const userRoleData = await userRoleDao.getByUserId(userData?.user_id);
      const dataToApi = { userData: userData, roleId: userRoleData?.role_id };
      result = { success: true, data: dataToApi };
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
      const userRoleData = await userRoleDao.getByUserId(userData?.user_id);
      const dataToApi = { userData: userData, roleId: userRoleData?.role_id };
      result = { success: true, data: dataToApi };
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
    let refreshToken: string;
    let result = null;
    const { email_id, user_password } = body;
    const dbUser = await userDao.getByEmailId(email_id);
    if (!dbUser || dbUser?.user_password !== md5(user_password)) {
      result = {
        success: false,
        message: 'Email id and password Wrong',
      };
      return result;
    }
    try {
      token = jwt.sign(
        { userId: dbUser.user_id, email: dbUser.email_id },
        process.env.API_ACCESS_TOKEN_SECRET_KEY,
        { expiresIn: '1m' }
      );
      refreshToken = jwt.sign(
        { userId: dbUser.user_id, email: dbUser.email_id },
        process.env.API_ACCESS_TOKEN_SECRET_KEY,
        { expiresIn: '10m' }
      );
    } catch (err) {
      console.log(' error occurred', err);
      return (result = {
        success: false,
        message: 'Error! Something went wrong',
      });
    }
    const fullName = dbUser?.first_name + ' ' + dbUser?.last_name;
    const loginResposne = {
      status: true,
      message: 'Success',
      token: `Bearer ${token}`,
      refreshToken: refreshToken,
      fullName: fullName,
      email: email_id,
    };
    res.send(loginResposne);
  } catch (error) {
    console.log('Error occurred in userLogin user service : ', error);
    const loginResposne = {
      status: false,
      message: 'something went wrong',
    };
  }
};

const refreshAccessToken = (refreshToken) => {
  try {
    const decodedPayload = verifyToken(refreshToken);

    const token = jwt.sign(
      { userId: decodedPayload['userId'], email: decodedPayload['email'] },
      process.env.API_ACCESS_TOKEN_SECRET_KEY,
      { expiresIn: '2m' }
    );
    return token;
  } catch (error) {
    console.log('Error occurred in refreshAccessToken: ', error);
    throw error;
  }
};

const verifyToken = (token) => {
  try {
    const decoded = jwt.verify(token, process.env.API_ACCESS_TOKEN_SECRET_KEY);
    return decoded; // Contains the original payload
  } catch (err) {
    console.error('Invalid token:', err.message);
    return null;
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
      const result = {
        success: true,
        message: 'User Data Deleted Successfully',
      };
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

/**
 * Method for updating user_status by user_id
 * @param body
 * @returns
 */
const updateStatus = async (body) => {
  try {
    const { user_id, user_status } = body;
    const userExist = await userDao.getById(user_id);
    if (!userExist) {
      const result = { success: false, message: 'User not exist' };
      return result;
    }
    const result = await userDao.updateStatus(user_id, user_status);
    const userData = { success: true, data: result };
    return userData;
  } catch (err) {
    console.log('Error occurred in User Service : Update Status Method');
  }
};

/**
 * Method for custom Search API
 * @param body
 * @returns
 */
const searchUser = async (body) => {
  try {
    const { size = 10, page = 0, sort = 'desc', global_filter } = body;

    let query = null;
    let countQuery = null;

    if (global_filter) {
      query = `select
          *
        from
          users u
        where
          concat(u.first_name, ' ', u.last_name) ilike '%${global_filter}%'
          or u.email_id ilike '%${global_filter}%'
          or u.contact_no ilike '%${global_filter}%'
          or u.address ilike '%${global_filter}%'
          or u.department ilike '%${global_filter}%' `;
      countQuery = `select
            count(*)
          from
            users u
          where
            concat(u.first_name, ' ', u.last_name) ilike '%${global_filter}%'
            or u.email_id ilike '%${global_filter}%'
            or u.contact_no ilike '%${global_filter}%'
            or u.address ilike '%${global_filter}%'
            or u.department ilike '%${global_filter}%' `;
    }

    const offset = page > 0 ? page * size : 0;

    query =
      query + `order by u.updated_date ${sort} limit ${size} offset ${offset}`;

    const data = await customQueryExecutor.customQueryExecutor(query);
    const count = await customQueryExecutor.customQueryExecutor(countQuery);

    const total_count = Number(count[0].count);

    const total_pages = total_count < size ? 1 : Math.ceil(total_count / size);

    const userData = {
      total_count: total_count,
      total_page: total_pages,
      size: size,
      content: data,
    };

    const result = {
      message: 'success',
      status: true,
      data: userData,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in searchUser user service : ', error);
    throw error;
  }
};

/**
 * Method for updating user_status by user_id
 * @param body
 * @returns
 */
const getDeletedUsers = async () => {
  try {
    const result = await userDao.getDeletedUsers();
    const userData = { success: true, data: result };
    return userData;
  } catch (err) {
    console.log('Error occurred in User Service : getDeletedUsers Method');
  }
};

/**
 * Method for custom filter API
 * @param body
 * @returns
 */
const customFilterUser = async (body) => {
  try {
    const {
      size = 10,
      page = 0,
      sort = 'desc',
      email_id = [],
      status = 'AC',
      name,
      contact_no,
    } = body;

    let query = `select * from users u where 1=1 and u.user_status='${status}' `;
    let countQuery = `select count(*) from users u where 1=1 and u.user_status='${status}' `;

    if (email_id.length > 0) {
      const emailList = email_id.map((email) => `'${email}'`).join(',');
      query = query + ` and u.email_id in (${emailList}) `;
      countQuery = countQuery + ` and u.email_id in (${emailList}) `;
    }

    if (name) {
      query =
        query + ` and concat(u.first_name,' ',u.last_name) ilike '%${name}%' `;
      countQuery =
        countQuery +
        ` and concat(u.first_name,' ',u.last_name) ilike '%${name}%' `;
    }

    if (contact_no) {
      query = query + ` and u.contact_no ilike '%${contact_no}%' `;
      countQuery = countQuery + ` and u.contact_no ilike '%${contact_no}%' `;
    }
    const offset = page > 0 ? page * size : 0;

    query =
      query + `order by u.updated_date ${sort} limit ${size} offset ${offset}`;

    const data = await customQueryExecutor.customQueryExecutor(query);
    const count = await customQueryExecutor.customQueryExecutor(countQuery);

    const total_count = Number(count[0].count);

    const total_pages = total_count < size ? 1 : Math.ceil(total_count / size);

    const userData = {
      total_count: total_count,
      total_page: total_pages,
      size: size,
      content: data,
    };

    const result = {
      message: 'success',
      status: true,
      data: userData,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in searchUser user service : ', error);
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
  deleteUser,
  updateStatus,
  searchUser,
  getDeletedUsers,
  customFilterUser,
  refreshAccessToken,
};
