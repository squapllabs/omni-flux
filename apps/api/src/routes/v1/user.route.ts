import express from 'express';
import {
  createUser,
  getAllUser,
  getByEmailId,
  getByUserId,
  userLogOut,
  userLogin,
} from '../../controller/user.controller';
import authMiddleware from '../../middleware/auth';
import {
  userLoginValidator,
  userCreateValidator,
} from '../../validations/users';
import { runValidation } from '../../validations/index';
const router = express.Router();

router.post(
  '/',
  authMiddleware,
  userCreateValidator,
  runValidation,
  createUser
);

router.get('/getById/:user_id', authMiddleware, getByUserId);

router.get('/getByEmailId/:email_id', authMiddleware, getByEmailId);

router.post('/login', userLoginValidator, runValidation, userLogin);

router.get('/getAll', authMiddleware, getAllUser);

router.get('/logout', userLogOut);

export default router;
