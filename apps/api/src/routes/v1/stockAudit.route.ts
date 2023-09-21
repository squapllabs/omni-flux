import express from 'express';
import authMiddleware from '../../middleware/auth';
import {
  stockAuditCreateValidator,
  stockAuditUpdateValidator,
} from '../../validations/stockAudit';
import {
  createStockAudit,
  deleteByStockAuditId,
  getAllStockAudits,
  getByStockAuditId,
  searchStockAudit,
  updateStockAudit,
} from '../../controller/stockAudit.controller';
import { runValidation } from '../../validations/index';

const router = express.Router();

router.post(
  '/',
  authMiddleware,
  stockAuditCreateValidator,
  runValidation,
  createStockAudit
);

router.put(
  '/',
  authMiddleware,
  stockAuditUpdateValidator,
  runValidation,
  updateStockAudit
);

router.get('/get-all', authMiddleware, getAllStockAudits);

router.get('/get/:stock_audit_id', authMiddleware, getByStockAuditId);

router.delete('/delete/:stock_audit_id', authMiddleware, deleteByStockAuditId);

router.post('/search', authMiddleware, searchStockAudit);

export default router;
