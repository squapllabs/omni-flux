import { useMutation, useQuery } from 'react-query';
import authService from '../service/auth-service';

const forgetPassword = () => {
  return useMutation({
    mutationFn: authService.forgetPassword,
  });
};

const loginAuth = () => {
  return useMutation({
    mutationFn: authService.loginAuth,
  });
};

const resetPassword = () => {
  return useMutation({
    mutationFn: authService.restePassword,
  });
};
const userlogout = () => {
  return useQuery([], () => authService.logout());
};
export { forgetPassword, loginAuth, resetPassword, userlogout };
