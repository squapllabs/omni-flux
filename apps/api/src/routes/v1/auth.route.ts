import express from 'express';
import { forgetPassword, editPassword } from '../../controller/auth.controller';
const router = express.Router();

router.post('/forgotPassword', forgetPassword);
router.put('/edit', editPassword);

export default router;
