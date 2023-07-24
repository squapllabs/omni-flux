import catchAsync from '../utils/catchAsync';
import * as itemTypeService from '../services/itemType.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';
const addItemType = catchAsync(async (req, res) => {
    const methodName = '/addItemType';
    try {
      const result = await itemTypeService.addItemType(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getAllItemType = catchAsync(async (req, res) => {
    const methodName = '/getAllItemType';
    try {
      const itemType = await itemTypeService.getAllItemType();
      res.send(itemType);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getByItemTypeId = catchAsync(async (req, res) => {
    const methodName = '/getByItemTypeId';
    try {
      const itemType = await itemTypeService.getById(req.params.item_type_id);
      res.send(itemType);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const deleteByItemTypeId = catchAsync(async (req, res) => {
    const methodName = '/deleteByItemTypeId';
    try {
      const itemType = await itemTypeService.deleteItemType(req.params.item_type_id);
      res.send(itemType);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const updateItemType = catchAsync(async (req, res) => {
    const methodName = '/updateItemType';
    try {
      const itemType = await itemTypeService.updateItemType(req.body);
      res.send(itemType);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  export{
    addItemType,
    getAllItemType,
    getByItemTypeId,
    deleteByItemTypeId,
    updateItemType
  }