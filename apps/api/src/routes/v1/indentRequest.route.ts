import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  indentRequestCreateValidator,
  indentRequestUpdateValidator,
} from '../../validations/indentRequest';
import {
  createIndentRequest,
  deleteByIndentRequestId,
  getAllIndentRequests,
  getByIndentRequestId,
  searchIndentRequest,
  updateIndentRequest,
} from '../../controller/indentRequest.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  indentRequestCreateValidator,
  runValidation,
  createIndentRequest
);

router.put(
  '/',
  authMiddleware,
  indentRequestUpdateValidator,
  runValidation,
  updateIndentRequest
);

router.get('/get-all', authMiddleware, getAllIndentRequests);

router.get('/get/:indent_request_id', authMiddleware, getByIndentRequestId);

router.delete(
  '/delete/:indent_request_id',
  authMiddleware,
  deleteByIndentRequestId
);

router.post('/search', authMiddleware, searchIndentRequest);

export default router;
