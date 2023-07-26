import axiosinterceptor from '../helper/custom_axios';
import { environment } from '../environment/environment';

const getAllUsers = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/user/getAll`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};

const getAllInactiveUsers = async () => {
  try {
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/user/getAll/IN`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getting all inactive users:', error);
    throw error;
  }
};

const getOneUser = async (values: any) => {
  try {
    const response = await axiosinterceptor.get(
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
    const response = await axiosinterceptor.get(
      `${environment.apiUrl}/user/getById/${values}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in getOneUser :', error);
    throw error;
  }
};

const createuser = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/user/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};
const updateUser = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.put(
      `${environment.apiUrl}/user/`,
      values
    );
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
  }
};
const deleteUser = async (id: number) => {
  try {
    const response = await axiosinterceptor.delete(
      `${environment.apiUrl}/user/delete/${id}`
    );
    return response.data;
  } catch (error) {
    console.log('Error in occur in delete user list :', error);
    throw error;
  }
};
export default {
  getAllUsers,
  getOneUser,
  getOneUserbyID,
  createuser,
  updateUser,
  deleteUser,
  getAllInactiveUsers
};
