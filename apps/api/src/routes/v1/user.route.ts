import express from 'express';
import {
  createUser,
  getAllUser,
  getByEmailId,
  getByUserId,
  userLogin,
} from '../../controller/user.controller';
const router = express.Router();

router.post('/', createUser);

router.get('/getById/:user_id', getByUserId);

router.get('/getByEmailId/:email_id', getByEmailId);

router.post('/login', userLogin);

router.get('/getAll', getAllUser);

export default router;
