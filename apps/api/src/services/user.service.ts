import userDao from '../dao/user.dao';
import userRoleDao from '../dao/userRole.dao';
import jwt from 'jsonwebtoken';
import md5 from 'md5';
import prisma from '../utils/prisma';
import { createUserBody, updateUserBody } from '../interfaces/user.Interface';
import userProfileDao from '../dao/userProfile.dao';

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
      profile_image_url,
      date_of_birth,
      gender,
      additional_info,
    } = body;

    const userEmailExist = await userDao.getByUniqueEmail(email_id);
    if (userEmailExist) {
      return (result = { success: false, message: 'email_id already exists' });
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
        if (userDetails) {
          const userProfileData = await userProfileDao.add(
            userDetails?.user_id,
            profile_image_url,
            date_of_birth,
            gender,
            address,
            additional_info,
            created_by,
            prisma
          );
          userDataWithRole.push({ userProfileData: userProfileData });
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
      profile_image_url,
      date_of_birth,
      gender,
      additional_info,
      is_two_factor,
    } = body;

    const userExist = await userDao.getById(user_id);
    if (!userExist) {
      return (result = { success: false, message: 'user id does not exists' });
    }

    const existingUserRoleData = await userRoleDao.getByUserId(user_id);
    const existingUserProfileData = await userProfileDao.getByUserId(user_id);

    const userDataWithRole = [];
    result = await prisma
      .$transaction(async (prisma) => {
        const userDetails = await userDao.edit(
          first_name,
          last_name,
          updated_by,
          user_id,
          department,
          is_two_factor,
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

        if (userDetails) {
          const userProfileData = await userProfileDao.edit(
            profile_image_url,
            date_of_birth,
            gender,
            address,
            additional_info,
            updated_by,
            existingUserProfileData?.user_profile_id,
            prisma
          );
          userDataWithRole.push({ userProfileData: userProfileData });
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
      const userProfileData = await userProfileDao.getByUserId(
        userData?.user_id
      );
      const dataToApi = {
        userData: userData,
        roleId: userRoleData?.role_id,
        userProfileData: userProfileData,
      };
      result = { message: 'success', status: true, data: dataToApi };
      return result;
    } else {
      result = { message: 'user_id does not exist', status: false, data: null };
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
        { expiresIn: '1d' }
      );
      refreshToken = jwt.sign(
        { userId: dbUser.user_id, email: dbUser.email_id },
        process.env.API_ACCESS_TOKEN_SECRET_KEY,
        { expiresIn: '100d' }
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
    return loginResposne;
  }
};

const refreshAccessToken = (refreshToken) => {
  try {
    const decodedPayload = verifyToken(refreshToken);

    const token = jwt.sign(
      { userId: decodedPayload['userId'], email: decodedPayload['email'] },
      process.env.API_ACCESS_TOKEN_SECRET_KEY,
      { expiresIn: '1d' }
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

// /**
//  * Method for Global Search API
//  * @param body
//  * @returns
//  */
// const searchUser = async (body) => {
//   try {
//     const { size = 10, page = 0, sort = 'desc', global_filter, status } = body;

//     let query = null;
//     let countQuery = null;

//     if (global_filter) {
//       query = prisma.users.findMany({
//         where: {
//           OR: [
//             { first_name: { contains: global_filter, mode: 'insensitive' } },
//             { last_name: { contains: global_filter, mode: 'insensitive' } },
//             { email_id: { contains: global_filter, mode: 'insensitive' } },
//             { contact_no: { contains: global_filter, mode: 'insensitive' } },
//             { department: { contains: global_filter, mode: 'insensitive' } },
//           ],
//           is_delete: status === 'IN' ? true : false,
//         },
//         orderBy: {
//           updated_date: sort,
//         },
//         take: size,
//         skip: page * size,
//       });

//       countQuery = prisma.users.count({
//         where: {
//           OR: [
//             { first_name: { contains: global_filter, mode: 'insensitive' } },
//             { last_name: { contains: global_filter, mode: 'insensitive' } },
//             { email_id: { contains: global_filter, mode: 'insensitive' } },
//             { contact_no: { contains: global_filter, mode: 'insensitive' } },
//             { department: { contains: global_filter, mode: 'insensitive' } },
//           ],
//           is_delete: status === 'IN' ? true : false,
//         },
//       });
//     }

//     const [data, count] = await Promise.all([query, countQuery]);

//     const total_count = count;
//     const total_pages = total_count < size ? 1 : Math.ceil(total_count / size);

//     const userData = {
//       total_count: total_count,
//       total_page: total_pages,
//       size: size,
//       content: data,
//     };

//     const result = {
//       message: 'success',
//       status: true,
//       data: userData,
//     };
//     return result;
//   } catch (error) {
//     console.log('Error occurred in searchUser user service:', error);
//     throw error;
//   }
// };

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
  } catch (error) {
    console.log('Error occurred in User Service : getDeletedUsers Method');
    throw error;
  }
};

/**
 * Method for custom filter API
 * @param body
 * @returns
 */
const customFilterUser = async (body) => {
  try {
    const offsetValue = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column;
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const filters = body.filters;
    const filterObj = {
      filterUser: {
        AND: [],
        is_delete: false,
        user_status: 'AC',
      },
    };

    for (const filter of filters) {
      const field_name = filter.field_name;
      const operator = filter.operator;
      const field_value = filter.field_value;

      await applyFilter(filterObj, field_name, operator, field_value);
    }

    const result = await userDao.customFilterUser(
      offsetValue,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempUserData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      limit: limit,
      content: data,
    };
    return tempUserData;
  } catch (error) {
    console.log('Error occurred in customFilterUser user service: ', error);
    throw error;
  }
};

/**
 * Method for Applying the Dynamic Filter Conditions
 */
const applyFilter = async (filterObj, field_name, operator, field_value) => {
  if (operator === 'Equal') {
    filterObj.filterUser[field_name] = field_value;
  } else if (operator === 'Not Equal') {
    filterObj.filterUser = { NOT: { [field_name]: field_value } };
  } else if (operator === 'Like') {
    filterObj.filterUser[field_name] = {
      contains: field_value,
      mode: 'insensitive',
    };
  } else if (operator === 'Not Like') {
    filterObj.filterUser = {
      NOT: {
        [field_name]: {
          contains: field_value,
          mode: 'insensitive',
        },
      },
    };
  } else if (operator === 'In') {
    filterObj.filterUser[field_name] = { in: field_value };
  } else if (operator === 'Not In') {
    filterObj.filterUser[field_name] = { notIn: field_value };
  } else if (operator === 'Is') {
    filterObj.filterUser[field_name] = null;
  } else {
    throw new Error(`Unsupported operator: ${operator}`);
  }
};

/**
 * Method to Get All Sales Person Users
 * @returns
 */
const getAllSalesPersonUsers = async () => {
  try {
    const result = await userDao.getAllSalesPersonUsers();
    const userData = { message: 'success', status: true, data: result };
    return userData;
  } catch (error) {
    console.log(
      'Error occurred in getAllSalesPersonUsers user service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search User - Pagination API
 * @returns
 */
const searchUser = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;
    const filterObj = {
      filterUser: {
        AND: [],
        OR: [
          { first_name: { contains: global_search, mode: 'insensitive' } },
          { last_name: { contains: global_search, mode: 'insensitive' } },
          { email_id: { contains: global_search, mode: 'insensitive' } },
          { contact_no: { contains: global_search, mode: 'insensitive' } },
          { department: { contains: global_search, mode: 'insensitive' } },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await userDao.searchUser(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempUserData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempUserData;
  } catch (error) {
    console.log('Error occurred in searchUser User service : ', error);
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
  getAllSalesPersonUsers,
};
