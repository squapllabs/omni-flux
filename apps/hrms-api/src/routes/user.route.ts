import express from 'express';
import { getAllUser } from '../controller/user.controller';
import authMiddleware from '../middleware/auth';

const router = express.Router();

router.get('/get-all/:user_status?', authMiddleware, getAllUser);

export default router;
