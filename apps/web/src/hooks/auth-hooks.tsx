import { useMutation, useQuery } from 'react-query';
import authService from '../service/auth-service';

const forgetPassword = () => {
  return useMutation({
    mutationFn: authService.forgetPassword,
  });
};

const generateOTP = () => {
  return useMutation({
    mutationFn:authService.generateOTP,
  });
};

const verifyOTP = () => {
  return useMutation({
    mutationFn:authService.verifyOTP,
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

const setTwoFA = () => {
  return useMutation({
    mutationFn: authService.setTwoFA,
  })
}

const userlogout = () => {
  return useQuery([], () => authService.logout());
};
export { forgetPassword, loginAuth, resetPassword, userlogout,generateOTP ,verifyOTP,setTwoFA};
