import { useMutation } from 'react-query';
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
export { forgetPassword, loginAuth, resetPassword };
