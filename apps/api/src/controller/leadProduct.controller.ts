import catchAsync from '../utils/catchAsync';
import * as leadProductService from '../services/leadProduct.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createLeadProduct = catchAsync(async (req, res) => {
  const methodName = '/createLeadProduct';
  try {
    const leadProduct = await leadProductService.createLeadProduct(req.body);
    res.send(leadProduct);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateLeadProduct = catchAsync(async (req, res) => {
  const methodName = '/updateLeadProduct';
  try {
    const leadProduct = await leadProductService.updateLeadProduct(req.body);
    res.send(leadProduct);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllLeadProduct = catchAsync(async (req, res) => {
  const methodName = '/getAllLeadProduct';
  try {
    const leadProduct = await leadProductService.getAllLeadProduct();
    res.send(leadProduct);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByLeadProductId = catchAsync(async (req, res) => {
  const methodName = '/getByLeadProductId';
  try {
    const leadProduct = await leadProductService.getById(
      req.params.lead_product_id
    );
    res.send(leadProduct);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByLeadProductId = catchAsync(async (req, res) => {
  const methodName = '/deleteByLeadProductId';
  try {
    const leadProduct = await leadProductService.deleteLeadProduct(
      req.params.lead_product_id
    );
    res.send(leadProduct);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createLeadProduct,
  updateLeadProduct,
  getAllLeadProduct,
  getByLeadProductId,
  deleteByLeadProductId,
};
