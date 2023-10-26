import catchAsync from '../utils/catchAsync';
import * as vendorQuotesService from '../services/vendorQuotes.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createVendorQuotes = catchAsync(async (req, res) => {
  const methodName = '/createVendorQuotes';
  try {
    const vendorQuotes = await vendorQuotesService.createVendorQuotes(req.body);
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateVendorQuotes = catchAsync(async (req, res) => {
  const methodName = '/updateVendorQuotes';
  try {
    const vendorQuotes = await vendorQuotesService.updateVendorQuotes(req.body);
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllVendorQuotes = catchAsync(async (req, res) => {
  const methodName = '/getAllVendorQuotes';
  try {
    const vendorQuotes = await vendorQuotesService.getAllVendorQuotes();
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByVendorQuotesId = catchAsync(async (req, res) => {
  const methodName = '/getByVendorQuotesId';
  try {
    const vendorQuotes = await vendorQuotesService.getById(
      req.params.vendor_quotes_id
    );
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByVendorQuotesId = catchAsync(async (req, res) => {
  const methodName = '/deleteByVendorQuotesId';
  try {
    const vendorQuotes = await vendorQuotesService.deleteVendorQuotes(
      req.params.vendor_quotes_id
    );
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchVendorQuotes = catchAsync(async (req, res) => {
  const methodName = '/searchVendorQuotes';
  try {
    const vendorQuotes = await vendorQuotesService.searchVendorQuotes(req.body);
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStatusAndDocument = catchAsync(async (req, res) => {
  const methodName = '/updateStatusAndDocument';
  try {
    const vendorQuotes = await vendorQuotesService.updateStatusAndDocument(
      req.body
    );
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseRequestIdAndVendorId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseRequestIdAndVendorId';
  try {
    const vendorQuotes =
      await vendorQuotesService.getByPurchaseRequestIdAndVendorId(
        req.params.purchase_request_id,
        req.params.vendor_id
      );
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByPurchaseRequestId = catchAsync(async (req, res) => {
  const methodName = '/getByPurchaseRequestId';
  try {
    const vendorQuotes = await vendorQuotesService.getByPurchaseRequestId(
      req.params.purchase_request_id
    );
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getVendorDetailsByPurchaseRequestId = catchAsync(async (req, res) => {
  const methodName = '/getVendorDetailsByPurchaseRequestId';
  try {
    const vendorQuotes =
      await vendorQuotesService.getVendorDetailsByPurchaseRequestId(
        req.params.purchase_request_id
      );
    res.send(vendorQuotes);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createVendorQuotes,
  getAllVendorQuotes,
  getByVendorQuotesId,
  deleteByVendorQuotesId,
  searchVendorQuotes,
  updateVendorQuotes,
  updateStatusAndDocument,
  getByPurchaseRequestIdAndVendorId,
  getByPurchaseRequestId,
  getVendorDetailsByPurchaseRequestId,
};
