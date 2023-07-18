import catchAsync from '../utils/catchAsync';
import * as stockService from '../services/stock.service'
import { handleError, ErrorHandler } from '../config/error';

const errorText = 'Error';
const createStock = catchAsync(async (req, res) => {
    const methodName = '/createStock';
    try {
      const result = await stockService.createStock(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getAllStock = catchAsync(async (req, res) => {
    const methodName = '/getAllStock';
    try {
      const result = await stockService.getAllStock();
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getByStockId = catchAsync(async (req, res) => {
    const methodName = '/getByStockId';
    try {
      const result = await stockService.getById(req.params.product_id);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const deleteByStockId = catchAsync(async (req, res) => {
    const methodName = '/deleteByStockId';
    try {
      const result = await stockService.deleteStock(req.params.product_id);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const updateStock = catchAsync(async (req, res) => {
    const methodName = '/updateStock';
    try {
      const result = await stockService.updateStock(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  export {
    updateStock,
    deleteByStockId,
    getAllStock,
    createStock,
    getByStockId
  }