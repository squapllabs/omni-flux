import catchAsync from '../utils/catchAsync';
import * as labourService from '../services/labour.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createLabour = catchAsync(async (req, res) => {
  const methodName = '/createLabour';
  try {
    const labour = await labourService.createLabour(req.body);
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateLabour = catchAsync(async (req, res) => {
  const methodName = '/updateLabour';
  try {
    const labour = await labourService.updateLabour(req.body);
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllLabours = catchAsync(async (req, res) => {
  const methodName = '/getAllLabours';
  try {
    const labour = await labourService.getAllLabours();
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByLabourId = catchAsync(async (req, res) => {
  const methodName = '/getByLabourId';
  try {
    const labour = await labourService.getById(req.params.labour_id);
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByLabourId = catchAsync(async (req, res) => {
  const methodName = '/deleteByLabourId';
  try {
    const labour = await labourService.deleteLabour(req.params.labour_id);
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchLabour = catchAsync(async (req, res) => {
  const methodName = '/searchLabour';
  try {
    const labour = await labourService.searchLabour(req.body);
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const checkDuplicateLabourType = catchAsync(async (req, res) => {
  const methodName = '/checkDuplicateLabourType';
  try {
    const labour = await labourService.checkDuplicateLabourType(
      req.params.labour_type
    );
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createLabour,
  updateLabour,
  getAllLabours,
  getByLabourId,
  deleteByLabourId,
  searchLabour,
  checkDuplicateLabourType,
};
