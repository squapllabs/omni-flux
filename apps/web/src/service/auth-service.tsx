import axios from 'axios';
import { environment } from '../environment/environment';
import { setItem } from '../helper/local-storage';
const forgetPassword = async (values: string) => {
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
const loginAuth = async (values: JSON) => {
  try {
    const response = await axios.post(
      `${environment.apiUrl}/user/login`,
      values
    );
    if (response?.data?.success === true) {
      const bearerToken = response?.data?.token;
      // const token = bearerToken.replace('Bearer ', '');
      setItem('Token', bearerToken);
      setItem('Name', response?.data?.fullName);
    }
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};

const restePassword = async (values: string) => {
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
