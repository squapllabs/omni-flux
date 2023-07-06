import { setToken } from '../redux/reducer';
import { useDispatch } from 'react-redux';

const useSetToken = () => {
  console.log('vvvvvvvvvvvvvvvvvvv');

  const dispatch = useDispatch();

  const setTokenValue = (key: string, value: string) => {
    try {
      const serializedValue = JSON.stringify(value);
      dispatch(setToken({ key: key, value: serializedValue }));
    } catch (error) {
      console.error(
        'Error setting item in useSetToken storage in redux store:',
        error
      );
    }
  };

  return setTokenValue;
};

export default useSetToken;

export const getItem = (key: string) => {
  try {
    const serializedValue = localStorage.getItem(key);
    return serializedValue ? JSON.parse(serializedValue) : null;
  } catch (error) {
    console.error('Error getting item from app local storage:', error);
    return null;
  }
};
