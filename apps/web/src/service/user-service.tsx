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

export default {
  getAllUsers,
};
