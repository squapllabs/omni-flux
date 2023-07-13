import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  clientCreateValidator,
  clientUpdateValidator,
} from '../../validations/client';
import {
  createClient,
  deleteByClientId,
  getAllClients,
  getByClientId,
  updateClient,
} from '../../controller/client.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  clientCreateValidator,
  runValidation,
  createClient
);

router.put(
  '/',
  authMiddleware,
  clientUpdateValidator,
  runValidation,
  updateClient
);

router.get('/getAll', authMiddleware, getAllClients);

router.get('/get/:client_id', authMiddleware, getByClientId);

router.delete('/delete/:client_id', authMiddleware, deleteByClientId);

export default router;
