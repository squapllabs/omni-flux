import { useMutation, useQuery } from 'react-query';
import authService from '../service/auth-service';

const useForgetPassword = () => {
  return useMutation({
    mutationFn: authService.forgetPassword,
  });
};

const useGenerateOTP = () => {
  return useMutation({
    mutationFn:authService.generateOTP,
  });
};

const useVerifyOTP = () => {
  return useMutation({
    mutationFn:authService.verifyOTP,
  });
};

const useLoginAuth = () => {
  return useMutation({
    mutationFn: authService.loginAuth,
  });
};

const useResetPassword = () => {
  return useMutation({
    mutationFn: authService.restePassword,
  });
};

const useSetTwoFA = () => {
  return useMutation({
    mutationFn: authService.setTwoFA,
  })
}

const useUserlogout = () => {
  return useQuery([], () => authService.logout());
};
export { useForgetPassword, useLoginAuth, useResetPassword, useUserlogout,useGenerateOTP ,useVerifyOTP,useSetTwoFA};
