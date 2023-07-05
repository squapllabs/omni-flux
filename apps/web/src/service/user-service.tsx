import axios from 'axios';
import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';
const getAllUsers = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/user/getAll`
    );
    return response;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};
const getOneUser = async (values: any) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/user/getByEmailId/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOneUser :', error);
    throw error;
  }
};
const getOneUserbyID = async (values: any) => {
  try {
    const response = await axios.get(
      `${environment.apiUrl}/user/getById/${values}`
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
  getOneUserbyID,
};
