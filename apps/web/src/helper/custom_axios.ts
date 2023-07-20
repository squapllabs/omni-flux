import axios, { AxiosError } from 'axios';
import { store, RootState } from '../redux/store';
import { getToken } from '../redux/reducer';
import authService from '../service/auth-service';

let encryptedData: { token: string; refreshToken: string } | null = null;
let loginToken: string | null;
let refreshToken: string | null;

const updateToken = () => {
  const state: RootState = store.getState();
  encryptedData = getToken(state, 'Data');
  loginToken = encryptedData !== null ? encryptedData['token'] : null;
  refreshToken = encryptedData !== null ? encryptedData['refreshToken'] : null;
  axios.defaults.headers.common['Authorization'] = loginToken;
};

updateToken();

store.subscribe(updateToken);

axios.interceptors.request.use(
  function (config) {
    config.headers = config.headers || {};
    config.headers['Authorization'] = loginToken;
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
  async function (error: AxiosError) {
    if (error.response?.status === 401) {
      const Object: any = {
        refreshToken: refreshToken,
      };
      const data = await authService.refreshTokenCall(Object);
      console.log("token to be set in cookies ==>",data.access_token);
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
