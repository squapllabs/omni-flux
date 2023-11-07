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
    const purchaseOrder = await purchaseOrderService.getAllPurchaseOrders(
      req.body
    );
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

const searchPurchaseOrderWithMultipleStatus = catchAsync(async (req, res) => {
  const methodName = '/searchPurchaseOrderWithMultipleStatus';
  try {
    const purchaseOrder = await purchaseOrderService.searchPurchaseOrderWithMultipleStatus(
      req.body
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const createPurchaseOrderWithItem = catchAsync(async (req, res) => {
  const methodName = '/createPurchaseOrderWithItem';
  try {
    const purchaseOrder =
      await purchaseOrderService.createPurchaseOrderWithItem(req.body);
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseRequestId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseRequestId';
  try {
    const purchaseOrder = await purchaseOrderService.getByPurchaseRequestId(
      req.params.purchase_request_id
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStatusAndDocument = catchAsync(async (req, res) => {
  const methodName = '/updateStatusAndDocument';
  try {
    const purchaseOrder = await purchaseOrderService.updateStatusAndDocument(
      req.body
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getPOStatistics = catchAsync(async (req, res) => {
  const methodName = '/getPOStatistics';
  try {
    const purchaseOrder = await purchaseOrderService.getPOStatistics();
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getPOReportData = catchAsync(async (req, res) => {
  const methodName = '/getPOReportData';
  try {
    const purchaseOrder = await purchaseOrderService.getPurchaseOrderReport(
      req.body
    );
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getRFQReportData = catchAsync(async (req, res) => {
  const methodName = '/getPOReportData';
  try {
    const purchaseOrder = await purchaseOrderService.getRFQReportData(req.body);
    res.send(purchaseOrder);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getPoChartData = catchAsync(async (req, res) => {
  const methodName = '/getPoChartData';
  try {
    const purchaseOrder = await purchaseOrderService.getPoChartData();
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
  createPurchaseOrderWithItem,
  getByPurchaseRequestId,
  updateStatusAndDocument,
  getPOStatistics,
  getPOReportData,
  searchPurchaseOrderWithMultipleStatus,
  getRFQReportData,
  getPoChartData,
};
