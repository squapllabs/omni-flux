import catchAsync from '../utils/catchAsync';
import * as purchaseOrderItemService from '../services/purchaseOrderItem.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createPurchaseOrderItem = catchAsync(async (req, res) => {
  const methodName = '/createPurchaseOrderItem';
  try {
    const purchaseOrderItem =
      await purchaseOrderItemService.createPurchaseOrderItem(req.body);
    res.send(purchaseOrderItem);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updatePurchaseOrderItem = catchAsync(async (req, res) => {
  const methodName = '/updatePurchaseOrderItem';
  try {
    const purchaseOrderItem =
      await purchaseOrderItemService.updatePurchaseOrderItem(req);
    res.send(purchaseOrderItem);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllPurchaseOrderItems = catchAsync(async (req, res) => {
  const methodName = '/getAllPurchaseOrderItems';
  try {
    const purchaseOrderItem =
      await purchaseOrderItemService.getAllPurchaseOrderItems();
    res.send(purchaseOrderItem);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseOrderItemId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseOrderItemId';
  try {
    const purchaseOrderItem = await purchaseOrderItemService.getById(
      req.params.purchase_order_item_id
    );
    res.send(purchaseOrderItem);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByPurchaseOrderItemId = catchAsync(async (req, res) => {
  const methodName = '/deleteByPurchaseOrderItemId';
  try {
    const purchaseOrderItem =
      await purchaseOrderItemService.deletePurchaseOrderItem(
        req.params.purchase_order_item_id
      );
    res.send(purchaseOrderItem);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchPurchaseOrderItem = catchAsync(async (req, res) => {
  const methodName = '/searchPurchaseOrderItem';
  try {
    const purchaseOrderItem =
      await purchaseOrderItemService.searchPurchaseOrderItem(req.body);
    res.send(purchaseOrderItem);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createPurchaseOrderItem,
  updatePurchaseOrderItem,
  getAllPurchaseOrderItems,
  getByPurchaseOrderItemId,
  deleteByPurchaseOrderItemId,
  searchPurchaseOrderItem,
};
