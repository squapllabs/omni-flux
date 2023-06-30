import axios from 'axios';
import axiosinterceptor from '../helper/custom_axios';
const getAllUsers = async () => {
  try {
    const response = await axiosinterceptor.get('http://localhost:8080/api/user/getAll');
    return response;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};
const loginAuth = async (values: any) => {
  try {
    console.log("values", values);
    const response = await axiosinterceptor.post('http://localhost:8080/api/user/login',values,
      {
        headers: {
          "token": 'success'
        }
      }
    );
    console.log('response.data', response.data);

    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};

const getOneUser = async (values: any) => {
  try {
    const response = await axiosinterceptor.post(`http://localhost:8080/api/user/getByEmailId/${values}`);
    console.log('response.data', response.data);

    return response.data;
  } catch (error) {
    console.log('Error in getOneUser :', error);
    throw error;
  }
};

export default {
  getAllUsers,
  getOneUser,
  loginAuth
};
