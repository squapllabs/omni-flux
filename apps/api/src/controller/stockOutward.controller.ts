import catchAsync from '../utils/catchAsync';
import * as stockOutwardService from '../services/stockOutward.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createStockOutward = catchAsync(async (req, res) => {
  const methodName = '/createStockOutward';
  try {
    const stockOutward = await stockOutwardService.createStockOutward(req.body);
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateStockOutward = catchAsync(async (req, res) => {
  const methodName = '/updateStockOutward';
  try {
    const stockOutward = await stockOutwardService.updateStockOutward(req.body);
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllStockOutwards = catchAsync(async (req, res) => {
  const methodName = '/getAllStockOutwards';
  try {
    const stockOutward = await stockOutwardService.getAllStockOutwards();
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByStockOutwardId = catchAsync(async (req, res) => {
  const methodName = '/getByStockOutwardId';
  try {
    const stockOutward = await stockOutwardService.getById(
      req.params.stock_outward_id
    );
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByStockOutwardId = catchAsync(async (req, res) => {
  const methodName = '/deleteByStockOutwardId';
  try {
    const stockOutward = await stockOutwardService.deleteStockOutward(
      req.params.stock_outward_id
    );
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchStockOutward = catchAsync(async (req, res) => {
  const methodName = '/searchStockOutward';
  try {
    const stockOutward = await stockOutwardService.searchStockOutward(req.body);
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const stockOutward = await stockOutwardService.getByProjectId(
      req.params.project_id
    );
    res.send(stockOutward);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createStockOutward,
  updateStockOutward,
  getAllStockOutwards,
  getByStockOutwardId,
  deleteByStockOutwardId,
  searchStockOutward,
  getByProjectId,
};
