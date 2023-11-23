import userDao from '../dao/user.dao';

/**
 * Method to Get All Users
 * @returns
 */
const getAllUser = async (user_status = 'AC') => {
  try {
    const result = await userDao.getAll(user_status);
    const userData = { message: 'success', status: true, data: result };
    return userData;
  } catch (error) {
    console.log('Error occurred in getAll user service : ', error);
    throw error;
  }
};

export { getAllUser };
