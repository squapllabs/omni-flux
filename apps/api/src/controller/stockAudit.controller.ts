import catchAsync from '../utils/catchAsync';
import * as stockAuditService from '../services/stockAudit.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createStockAudit = catchAsync(async (req, res) => {
  const methodName = '/createStockAudit';
  try {
    const stockAudit = await stockAuditService.createStockAudit(req.body);
    res.send(stockAudit);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStockAudit = catchAsync(async (req, res) => {
  const methodName = '/updateStockAudit';
  try {
    const stockAudit = await stockAuditService.updateStockAudit(req.body);
    res.send(stockAudit);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllStockAudits = catchAsync(async (req, res) => {
  const methodName = '/getAllStockAudits';
  try {
    const stockAudit = await stockAuditService.getAllStockAudits();
    res.send(stockAudit);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByStockAuditId = catchAsync(async (req, res) => {
  const methodName = '/getByStockAuditId';
  try {
    const stockAudit = await stockAuditService.getById(
      req.params.stock_audit_id
    );
    res.send(stockAudit);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByStockAuditId = catchAsync(async (req, res) => {
  const methodName = '/deleteByStockAuditId';
  try {
    const stockAudit = await stockAuditService.deleteStockAudit(
      req.params.stock_audit_id
    );
    res.send(stockAudit);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchStockAudit = catchAsync(async (req, res) => {
  const methodName = '/searchStockAudit';
  try {
    const stockAudit = await stockAuditService.searchStockAudit(req.body);
    res.send(stockAudit);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createStockAudit,
  updateStockAudit,
  getAllStockAudits,
  getByStockAuditId,
  deleteByStockAuditId,
  searchStockAudit,
};
