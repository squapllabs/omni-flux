import catchAsync from '../utils/catchAsync';
import * as uomService from '../services/uom.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createUom = catchAsync(async (req, res) => {
  const methodName = '/createUom';
  try {
    const uom = await uomService.createUom(req.body);
    res.send(uom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateUom = catchAsync(async (req, res) => {
  const methodName = '/updateUom';
  try {
    const uom = await uomService.updateUom(req.body);
    res.send(uom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllUom = catchAsync(async (req, res) => {
  const methodName = '/getAllUom';
  try {
    const uom = await uomService.getAllUom();
    res.send(uom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByUomId = catchAsync(async (req, res) => {
  const methodName = '/getByUomId';
  try {
    const uom = await uomService.getById(req.params.uom_id);
    res.send(uom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByUomId = catchAsync(async (req, res) => {
  const methodName = '/deleteByUomId';
  try {
    const uom = await uomService.deleteUom(req.params.uom_id);
    res.send(uom);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { createUom, updateUom, getAllUom, getByUomId, deleteByUomId };
