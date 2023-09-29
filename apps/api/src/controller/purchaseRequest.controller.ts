import catchAsync from '../utils/catchAsync';
import * as purchaseRequestService from '../services/purchaseRequest.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createPurchaseRequest = catchAsync(async (req, res) => {
  const methodName = '/createPurchaseRequest';
  try {
    const purchaseRequest = await purchaseRequestService.createPurchaseRequest(
      req.body
    );
    res.send(purchaseRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updatePurchaseRequest = catchAsync(async (req, res) => {
  const methodName = '/updatePurchaseRequest';
  try {
    const purchaseRequest = await purchaseRequestService.updatePurchaseRequest(
      req
    );
    res.send(purchaseRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllPurchaseRequests = catchAsync(async (req, res) => {
  const methodName = '/getAllPurchaseRequests';
  try {
    const purchaseRequest =
      await purchaseRequestService.getAllPurchaseRequests();
    res.send(purchaseRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseRequestId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseRequestId';
  try {
    const purchaseRequest = await purchaseRequestService.getById(
      req.params.purchase_request_id
    );
    res.send(purchaseRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByPurchaseRequestId = catchAsync(async (req, res) => {
  const methodName = '/deleteByPurchaseRequestId';
  try {
    const purchaseRequest = await purchaseRequestService.deletePurchaseRequest(
      req.params.purchase_request_id
    );
    res.send(purchaseRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchPurchaseRequest = catchAsync(async (req, res) => {
  const methodName = '/searchPurchaseRequest';
  try {
    const purchaseRequest = await purchaseRequestService.searchPurchaseRequest(
      req.body
    );
    res.send(purchaseRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createPurchaseRequest,
  updatePurchaseRequest,
  getAllPurchaseRequests,
  getByPurchaseRequestId,
  deleteByPurchaseRequestId,
  searchPurchaseRequest,
};
