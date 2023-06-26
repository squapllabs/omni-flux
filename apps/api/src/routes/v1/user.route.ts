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

router.get('/getById/:id', getByUserId);

router.get('/getByEmailId/:email', getByEmailId);

router.post('/login', userLogin);

router.get('/getAll', getAllUser);

export default router;
