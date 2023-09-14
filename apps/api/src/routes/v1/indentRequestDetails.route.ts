import express from 'express';
import authMiddleware from '../../middleware/auth';
import { searchIndentRequestDetails } from '../../controller/indentRequestDetails.controller';

const router = express.Router();

router.post('/search', authMiddleware, searchIndentRequestDetails);

export default router;
