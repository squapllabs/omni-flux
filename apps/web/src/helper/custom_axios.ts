import axios, { AxiosError, AxiosHeaders } from 'axios';
// import GetToken from '../redux/tokenUtils';
import store, { RootState } from '../redux/store';
import { getToken } from '../redux/reducer';

// const encryptedData = GetToken(store.getState() as RootState);
let encryptedData: string | null;
// store.subscribe(() => {
//   const state: RootState = store.getState();
//   encryptedData = getToken(state, 'Token');
//   console.log('Updated token:', encryptedData);
// });

const updateToken = () => {
  const state: RootState = store.getState();
  encryptedData = getToken(state, 'Token');
  console.log('Updated token:', encryptedData);

  axios.defaults.headers.common['Authorization'] = encryptedData;
};

updateToken();

store.subscribe(updateToken);

axios.interceptors.request.use(
  function (config) {
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
