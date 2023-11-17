/* eslint-disable @typescript-eslint/no-explicit-any */
import React, { useEffect, useState, useRef } from 'react';
import { useNavigate } from 'react-router-dom';
import { store, RootState } from '../redux/store';
import { getToken } from '../redux/reducer';
const ProtectedRoute: React.FC = (props: any) => {
  let encryptedData: { token: string } | null = null;
  // let loginToken: string | null;
  const state: RootState = store.getState();
  encryptedData = getToken(state, 'Data');
  const loginTokenRef: any = useRef(null);
  const navigate = useNavigate();
  const [isLoggedIn, setIsLoggedIn] = useState<boolean>(false);

  useEffect(() => {
    const checkUserToken = () => {
      const loginToken = encryptedData !== null ? encryptedData['token'] : null;
      loginTokenRef.current = loginToken;
      if (!loginTokenRef || loginTokenRef === 'undefined') {
        setIsLoggedIn(false);
        return navigate('/');
      }
      setIsLoggedIn(true);
    };
    checkUserToken();
  }, [isLoggedIn]);

  return <div>{isLoggedIn ? props.children : null}</div>;
};

export default ProtectedRoute;
