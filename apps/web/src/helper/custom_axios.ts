import axios, { AxiosError } from 'axios';
import { getItem } from './local-storage';

axios.interceptors.request.use(
  function (config) {
    const encryptedData = getItem('Token');
    config.headers = config.headers || {};
    config.headers['Authorization'] = encryptedData;
    return config;
  },
  function (error: AxiosError) {
    console.log('err in interceptor request axios file', error);
    return Promise.reject(error);
  }
);

axios.interceptors.response.use(
  function (response) {
    return response;
  },
  function (error: AxiosError) {
    if (error.response?.status === 401) {
      console.log('Unauthorized access!', error);
    } else if (error.response?.status === 404) {
      console.log('Not found!');
    } else {
      console.log('Error:', error);
    }
    return Promise.reject(error);
  }
);

export default axios;
