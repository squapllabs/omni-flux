import axios, { AxiosError, AxiosRequestConfig, AxiosResponse } from 'axios';
import { getCookie } from '../helper/session';

axios.interceptors.request.use(function (config) {
  const encryptedData = getCookie("logintoken");
  config.headers = config.headers || {}; 
  config.headers['Authorization'] = encryptedData;
  return config;
}, function (error: AxiosError) {
  console.log("err in interceptor request axios file", error);
  return Promise.reject(error);
});

axios.interceptors.response.use(function (response) {
  return response;
}, function (error: AxiosError) {
  if (error.response?.status === 401) {
    console.log('Unauthorized access!', error);
  } else if (error.response?.status === 404) {
    console.log('Not found!');
  } else {
    console.log('Error:', error);
  }
  return Promise.reject(error);
});

export default axios;