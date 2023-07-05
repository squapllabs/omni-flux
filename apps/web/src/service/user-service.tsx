import axiosinterceptor from '../helper/custom_axios';
import { setItem } from '../helper/local-storage';

const getAllUsers = async () => {
  try {
    const response = await axiosinterceptor.get(
      'http://localhost:8080/api/user/getAll'
    );
    return response;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};

const loginAuth = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      'http://localhost:8080/api/user/login',
      values
    );

    if (response?.data?.success === true) {
      setItem('Token', response?.data?.token);
      setItem('Name', response?.data?.fullName);
    }
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};

const getOneUser = async (values: string) => {
  try {
    const response = await axiosinterceptor.post(
      `http://localhost:8080/api/user/getByEmailId/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOneUser :', error);
    throw error;
  }
};

export default {
  getAllUsers,
  getOneUser,
  loginAuth,
};
