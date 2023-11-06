import catchAsync from '../utils/catchAsync';
import * as purchaseOrderInvoiceService from '../services/purchaseOrderInvoice.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createPurchaseOrderInvoice = catchAsync(async (req, res) => {
  const methodName = '/createPurchaseOrderInvoice';
  try {
    const purchaseOrderInvoice =
      await purchaseOrderInvoiceService.createPurchaseOrderInvoice(req.body);
    res.send(purchaseOrderInvoice);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updatePurchaseOrderInvoice = catchAsync(async (req, res) => {
  const methodName = '/updatePurchaseOrderInvoice';
  try {
    const purchaseOrderInvoice =
      await purchaseOrderInvoiceService.updatePurchaseOrderInvoice(req.body);
    res.send(purchaseOrderInvoice);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllPurchaseOrderInvoices = catchAsync(async (req, res) => {
  const methodName = '/getAllPurchaseOrderInvoices';
  try {
    const purchaseOrderInvoice =
      await purchaseOrderInvoiceService.getAllPurchaseOrderInvoices();
    res.send(purchaseOrderInvoice);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseOrderInvoiceId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseOrderInvoiceId';
  try {
    const purchaseOrderInvoice = await purchaseOrderInvoiceService.getById(
      req.params.purchase_order_invoice_id
    );
    res.send(purchaseOrderInvoice);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseOrderId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseOrderId';
  try {
    const purchaseOrderInvoice = await purchaseOrderInvoiceService.getByPOId(
      req.params.purchase_order_id
    );
    res.send(purchaseOrderInvoice);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByPurchaseOrderInvoiceId = catchAsync(async (req, res) => {
  const methodName = '/deleteByPurchaseOrderInvoiceId';
  try {
    const purchaseOrderInvoice =
      await purchaseOrderInvoiceService.deletePurchaseOrderInvoice(
        req.params.purchase_order_invoice_id
      );
    res.send(purchaseOrderInvoice);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchPurchaseOrderInvoice = catchAsync(async (req, res) => {
  const methodName = '/searchPurchaseOrderInvoice';
  try {
    const purchaseOrderInvoice =
      await purchaseOrderInvoiceService.searchPurchaseOrderInvoice(req.body);
    res.send(purchaseOrderInvoice);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createPurchaseOrderInvoice,
  updatePurchaseOrderInvoice,
  getAllPurchaseOrderInvoices,
  getByPurchaseOrderInvoiceId,
  getByPurchaseOrderId,
  deleteByPurchaseOrderInvoiceId,
  searchPurchaseOrderInvoice,
};
