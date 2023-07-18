import catchAsync from '../utils/catchAsync';
import * as hsnCodeService from '../services/hsnCode.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createHsnCode = catchAsync(async (req, res) => {
  const methodName = '/createHsnCode';
  try {
    const hsnCode = await hsnCodeService.createHsnCode(req.body);
    res.send(hsnCode);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateHsnCode = catchAsync(async (req, res) => {
  const methodName = '/updateHsnCode';
  try {
    const hsnCode = await hsnCodeService.updateHsnCode(req.body);
    res.send(hsnCode);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllHsnCode = catchAsync(async (req, res) => {
  const methodName = '/getAllHsnCode';
  try {
    const hsnCode = await hsnCodeService.getAllHsnCode();
    res.send(hsnCode);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByHsnCodeId = catchAsync(async (req, res) => {
  const methodName = '/getByHsnCodeId';
  try {
    const hsnCode = await hsnCodeService.getById(req.params.hsn_code_id);
    res.send(hsnCode);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByHsnCodeId = catchAsync(async (req, res) => {
  const methodName = '/deleteByHsnCodeId';
  try {
    const hsnCode = await hsnCodeService.deleteHsnCode(req.params.hsn_code_id);
    res.send(hsnCode);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByHsnCode = catchAsync(async (req, res) => {
  const methodName = '/getByHsnCode';
  try {
    const hsnCode = await hsnCodeService.getByCode(req.params.code);
    res.send(hsnCode);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createHsnCode,
  updateHsnCode,
  getAllHsnCode,
  getByHsnCodeId,
  deleteByHsnCodeId,
  getByHsnCode,
};
