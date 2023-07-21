import axios, { AxiosError } from 'axios';
import { store, RootState } from '../redux/store';
import { getToken } from '../redux/reducer';
import authService from '../service/auth-service';
import { setToken } from '../redux/reducer';

let encryptedData: { token: string; refreshToken: string } | null = null;
let loginToken: string | null;
let refreshToken: string | null;

const updateToken = () => {
  const state: RootState = store.getState();
  encryptedData = getToken(state, 'Data');
  loginToken = encryptedData !== null ? encryptedData['token'] : null;
  refreshToken = encryptedData !== null ? encryptedData['refreshToken'] : null;
  console.log('refresh token', refreshToken);

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
    // const dispatch = useDispatch();
    console.log('401 innn');
    console.log('401 inn refresh token', refreshToken);

    if (error.response?.status === 401) {
      const Object: any = {
        refreshToken: refreshToken,
      };
      console.log('object==>', Object);

      const data = await authService.refreshTokenCall(Object);
      console.log('data in refresh call file====>', data);
      console.log("check encrypt data-->", encryptedData)
      await store.dispatch(
        setToken({ key: 'Data', value: { ...encryptedData, token: data?.accessToken, refreshToken: data?.refreshToken } })
      );
      window.location.reload()
      // window.location.href = 'http://localhost:4200/uom-list'
      console.log('Unauthorized access!', error);
    } else {
      console.log('Error:', error);
      await store.dispatch(
        setToken({ key: 'Data', value: { ...encryptedData, token: null, refreshToken: null, } })
      );
    }
    return Promise.reject(error);
  }
);

export default axios;
