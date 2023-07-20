import axios from 'axios';
import { environment } from '../environment/environment';

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
      `${environment.apiUrl}/user/login`,
      values
    );
    console.log("check request data-->",values);  
    console.log("check login response data$$$$->",response.data)

    const loginValidateRequest={
      accessToken:response.data["accessToken"],
      email_id:values["email_id"],
      refreshToken:response.data["refreshToken"],
      isRememberMe:values["is_remember_me"]
    }

    console.log("login validate request -->",loginValidateRequest)

    await axios.post(
      `${environment.apiUrl}/user/loginValidate`,
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
    const response = await axios.get(`${environment.apiUrl}/user/logout`, {
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

export default {
  forgetPassword,
  loginAuth,
  restePassword,
  logout,
};
