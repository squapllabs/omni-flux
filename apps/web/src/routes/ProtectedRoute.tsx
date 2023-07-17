/* eslint-disable @typescript-eslint/no-explicit-any */
import React, { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { store, RootState } from '../redux/store';
import { getToken } from '../redux/reducer';
const ProtectedRoute: React.FC = (props: any) => {
  let encryptedData: { token: string } | null = null;
  let loginToken: string | null;
  const navigate = useNavigate();
  const [isLoggedIn, setIsLoggedIn] = useState<boolean>(false);
  const checkUserToken = () => {
    const state: RootState = store.getState();
    encryptedData = getToken(state, 'Data');
    loginToken = encryptedData !== null ? encryptedData['token'] : null;
    if (!loginToken || loginToken === 'undefined') {
      setIsLoggedIn(false);
      return navigate('/');
    }
    setIsLoggedIn(true);
  };

  useEffect(() => {
    checkUserToken();
  }, [isLoggedIn]);

  return <React.Fragment>{isLoggedIn ? props.children : null}</React.Fragment>;
};

export default ProtectedRoute;
