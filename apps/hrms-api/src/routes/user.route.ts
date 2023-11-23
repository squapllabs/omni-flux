import express from 'express';
import { getAllUser } from '../controller/user.controller';

const router = express.Router();

router.get('/get-all/:user_status?', getAllUser);

export default router;
