import express from 'express';
import { runValidation } from '../validations';
import { loginValidator } from '../validations/users';
import { login } from '../controllers/user.controller';
const router = express.Router();
router.post('/login', loginValidator, runValidation, login);
export default router;
