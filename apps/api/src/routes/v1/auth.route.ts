import express from 'express';
import {
  forgetPassword,
  editPassword,
  isLoggedIn,
  logOut,
  login,
  loginValidate,
  refreshToken,
  generateOTP,
  verifyOTP,
} from '../../controller/auth.controller';
import { runValidation } from '../../validations';
import { userLoginValidator } from '../../validations/users';
import { validateCookie } from '../../utils/helper';
const router = express.Router();

router.post('/forgotPassword', forgetPassword);
router.put('/edit', editPassword);
router.post('/login', userLoginValidator, runValidation, login);
router.post('/loginValidate', validateCookie, loginValidate);
router.post('/refreshToken', refreshToken);
router.get('/logout', logOut);
router.get('/isLoggedIn', isLoggedIn);
router.post('/generate-otp', generateOTP);
router.post('/verify-otp', verifyOTP);

export default router;
