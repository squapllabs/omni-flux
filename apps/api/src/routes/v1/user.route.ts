import express from 'express';
import {
  createUser,
  deleteUser,
  getAllUser,
  getByEmailId,
  getByUserId,
  updateUser,
  userLogOut,
  userLogin,
} from '../../controller/user.controller';
import authMiddleware from '../../middleware/auth';
import {
  userLoginValidator,
  userCreateValidator,
  userUpdateValidator,
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

router.put('/', authMiddleware, userUpdateValidator, runValidation, updateUser);

router.get('/getById/:user_id', authMiddleware, getByUserId);

router.get('/getByEmailId/:email_id', authMiddleware, getByEmailId);

router.post('/login', userLoginValidator, runValidation, userLogin);

router.get('/getAll/:user_status?', authMiddleware, getAllUser);

router.get('/logout', userLogOut);

router.delete('/delete/:user_id', deleteUser);

export default router;
