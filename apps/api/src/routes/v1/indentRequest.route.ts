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
  getByProjectId,
  searchIndentRequest,
  updateIndentRequest,
  updateStatus,
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

router.get('/get-by-project-id/:project_id', authMiddleware, getByProjectId);

router.put('/update-status', authMiddleware, updateStatus);

export default router;
