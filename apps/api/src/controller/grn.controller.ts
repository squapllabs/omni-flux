import catchAsync from '../utils/catchAsync';
import * as grnService from '../services/grn.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createGrn = catchAsync(async (req, res) => {
  const methodName = '/createGrn';
  try {
    const grn = await grnService.createGrn(req.body);
    res.send(grn);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllGrns = catchAsync(async (req, res) => {
  const methodName = '/getAllGrns';
  try {
    const grn = await grnService.getAllGrns();
    res.send(grn);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByGrnId = catchAsync(async (req, res) => {
  const methodName = '/getByGrnId';
  try {
    const grn = await grnService.getById(req.params.grn_id);
    res.send(grn);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchGrn = catchAsync(async (req, res) => {
  const methodName = '/searchGrn';
  try {
    const grn = await grnService.searchGrn(req.body);
    res.send(grn);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseOrderId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseOrderId';
  try {
    const grn = await grnService.getByPurchaseOrderId(
      req.params.purchase_order_id
    );
    res.send(grn);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { createGrn, getAllGrns, getByGrnId, searchGrn, getByPurchaseOrderId };
