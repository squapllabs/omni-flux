import express from 'express';
import {
  createUser,
  customFilterUser,
  deleteUser,
  getAllUser,
  getByEmailId,
  getByUserId,
  getDeletedUsers,
  searchUser,
  updateStatus,
  updateUser,
  userLogOut,
  userLogin,
  isLoggedIn,
  loginValidate,
  refreshToken
} from '../../controller/user.controller';
import authMiddleware from '../../middleware/auth';
import {
  userLoginValidator,
  userCreateValidator,
  userUpdateValidator,
  userUpdateStatusValidator,
} from '../../validations/users';
import { runValidation } from '../../validations/index';
import { validateCookie } from './../../utils/helper'
const router = express.Router();

router.post(
  '/',
  authMiddleware,
  userCreateValidator,
  runValidation,
  createUser
);

router.put('/', authMiddleware, userUpdateValidator, runValidation, updateUser);
router.get('/getById/:user_id', authMiddleware, getByUserId);
router.get('/getByEmailId/:email_id', authMiddleware, getByEmailId);
router.post('/login', userLoginValidator, runValidation, userLogin);
router.post('/loginValidate', validateCookie, loginValidate);
router.post('/refreshToken', refreshToken);
router.get('/getAll/:user_status?', authMiddleware, getAllUser);
router.get('/logout', userLogOut);
router.get('/isLoggedIn', isLoggedIn);
router.delete('/delete/:user_id', deleteUser);

router.put(
  '/updateStatus',
  authMiddleware,
  userUpdateStatusValidator,
  runValidation,
  updateStatus
);

router.post('/searchUser', authMiddleware, searchUser);

router.get('/getDeletedUsers', authMiddleware, getDeletedUsers);

router.post('/customFilter', authMiddleware, customFilterUser);

export default router;
