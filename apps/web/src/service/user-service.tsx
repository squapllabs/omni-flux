import axios from 'axios';

const getAllUsers = async () => {
  try {
    const response = await axios.get('http://localhost:8080/api/user/getAll');
    return response;
  } catch (error) {
    console.log('Error in getting all users:', error);
    throw error;
  }
};
const loginAuth = async (values: any) => {
  try {
    const response = await axios.post('http://localhost:8080/api/user/login', values);
    console.log('response.data', response.data);

    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};

const getOneUser = async (values: any) => {
  try {
    const response = await axios.post(`http://localhost:8080/api/user/getByEmailId/${values}`);
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
