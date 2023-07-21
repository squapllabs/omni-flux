import axios from 'axios';
import { environment } from '../environment/environment';
import axiosinterceptor from '../helper/custom_axios';

const forgetPassword = async (values: any) => {
  console.log('values', values);

  try {
    const response = await axios.post(
      `${environment.apiUrl}/auth/forgotPassword/`,
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
      `${environment.apiUrl}/auth/login`,
      values
    );
    console.log("check login response data$$$$->", response.data)
    const loginValidateRequest = {
      accessToken: response.data["token"],
      email_id: values["email_id"],
      refreshToken: response.data["refreshToken"],
      isRememberMe: values["is_remember_me"]
    }

    await axios.post(
      `${environment.apiUrl}/auth/loginValidate`,
      loginValidateRequest
    );

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

const logout = async () => {
  try {
    const response = await axios.get(`${environment.apiUrl}/auth/logout`, {
      headers: {
        token: 'success',
      },
    });
    return response.data;
  } catch (error) {
    console.log('Error in loginAuth :', error);
    throw error;
  }
};

const refreshTokenCall = async (values: JSON) => {
  try {
    const response = await axiosinterceptor.post(
      `${environment.apiUrl}/auth/refreshToken`,
      values
    );
    console.log("refresh token call----->", response.data)
    return response.data;
  } catch (error) {
    console.log('Error in refreshTokenCall :', error);
    throw error;
  }
};
export default {
  forgetPassword,
  loginAuth,
  restePassword,
  logout,
  refreshTokenCall
};
