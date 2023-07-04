import express from 'express';
import { forgetPassword } from '../../controller/auth.controller';
const router = express.Router();

router.post('/forgetPassword', forgetPassword);

export default router;