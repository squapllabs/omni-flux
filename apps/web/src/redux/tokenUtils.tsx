import { RootState } from '../redux/store';
import { getToken } from './reducer';

const GetToken = (store: RootState) => {
  const token = getToken(store, 'Token');

  console.log('Token in GetToken Function', token);

  return token;
};

export default { GetToken };
