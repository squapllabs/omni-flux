import catchAsync from '../utils/catchAsync';
import * as purchaseOrderService from '../services/purchaseOrder.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createPurchaseOrder = catchAsync(async (req, res) => {
  const methodName = '/createPurchaseOrder';
  try {
    const purchaseOrder = await purchaseOrderService.createPurchaseOrder(
      req.body
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updatePurchaseOrder = catchAsync(async (req, res) => {
  const methodName = '/updatePurchaseOrder';
  try {
    const purchaseOrder = await purchaseOrderService.updatePurchaseOrder(
      req.body
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllPurchaseOrders = catchAsync(async (req, res) => {
  const methodName = '/getAllPurchaseOrders';
  try {
    const purchaseOrder = await purchaseOrderService.getAllPurchaseOrders();
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseOrderId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseOrderId';
  try {
    const purchaseOrder = await purchaseOrderService.getById(
      req.params.purchase_order_id
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByPurchaseOrderId = catchAsync(async (req, res) => {
  const methodName = '/deleteByPurchaseOrderId';
  try {
    const purchaseOrder = await purchaseOrderService.deletePurchaseOrder(
      req.params.purchase_order_id
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchPurchaseOrder = catchAsync(async (req, res) => {
  const methodName = '/searchPurchaseOrder';
  try {
    const purchaseOrder = await purchaseOrderService.searchPurchaseOrder(
      req.body
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createPurchaseOrder,
  updatePurchaseOrder,
  getAllPurchaseOrders,
  getByPurchaseOrderId,
  deleteByPurchaseOrderId,
  searchPurchaseOrder,
};
