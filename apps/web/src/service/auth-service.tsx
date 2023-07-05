import axios from 'axios';
import { environment } from '../environment/environment';
const forgetPassword = async (values: any) => {
  try {
    const response = await axios.post(
      `${environment.apiUrl}/auth/forgetPassword`,
      values,
      {
        headers: {
          token: 'success',
        },
      }
    );
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};
const loginAuth = async (values: any) => {
  try {
    const response = await axios.post(
      `${environment.apiUrl}/user/login`,
      values,
      {
        headers: {
          token: 'success',
        },
      }
    );
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};

const restePassword = async (values: any) => {
  try {
    const response = await axios.put(
      `${environment.apiUrl}/auth/edit/`,
      values,
      {
        headers: {
          token: 'success',
        },
      }
    );
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};

export default {
  forgetPassword,
  loginAuth,
  restePassword,
};
