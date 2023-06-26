import express from 'express';
import {getAllUser} from '../../controller/user.controller';
const router = express.Router();

router.get(
    '/getAll',
    getAllUser
);

export default router ;