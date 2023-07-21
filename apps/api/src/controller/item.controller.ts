import catchAsync from '../utils/catchAsync';
import * as itemService from '../services/item.service';
import { handleError, ErrorHandler } from '../config/error';

const errorText = 'Error';
const addItem = catchAsync(async (req, res) => {
    const methodName = '/addItem';
    try {
      const result = await itemService.addItem(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const addBulkItem = catchAsync(async (req, res) => {
    const methodName = '/addBulkItem';
    try {
      const result = await itemService.createItemBulk(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getAllItem = catchAsync(async (req, res) => {
    const methodName = '/getAllItem';
    try {
      const item = await itemService.getAllItem(req.body);
      res.send(item);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getAllItemBySearch = catchAsync(async (req, res) => {
    const methodName = '/getAllItemBySearch';
    try {
      const result = await itemService.getAllItemBySearch(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getByItemId = catchAsync(async (req, res) => {
    const methodName = '/getByItemId';
    try {
      const item = await itemService.getById(req.params.item_id);
      res.send(item);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const deleteByItemId = catchAsync(async (req, res) => {
    const methodName = '/deleteByItemId';
    try {
      const item = await itemService.deleteItem(req.params.item_id);
      res.send(item);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const updateItem = catchAsync(async (req, res) => {
    const methodName = '/updateItem';
    try {
      const item = await itemService.updateItem(req.body);
      res.send(item);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  export {
   addItem,
   deleteByItemId,
   getAllItem,
   getByItemId,
   updateItem,
   addBulkItem,
   getAllItemBySearch
  }