import express from 'express';
import {
  createUser,
  getAllUser,
  getByEmailId,
  getByUserId,
  userLogin,
} from '../../controller/user.controller';
import authMiddleware from '../../middleware/auth';
import { userLoginValidator } from '../../validations/users';
const router = express.Router();

router.post('/', authMiddleware, createUser);

router.get('/getById/:user_id', authMiddleware, getByUserId);

router.get('/getByEmailId/:email_id', authMiddleware, getByEmailId);

router.post('/login', userLoginValidator, userLogin);

router.get('/getAll', authMiddleware, getAllUser);

export default router;
