import catchAsync from '../utils/catchAsync';
import * as gstService from '../services/gst.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createGst = catchAsync(async (req, res) => {
  const methodName = '/createGst';
  try {
    const gst = await gstService.createGst(req.body);
    res.send(gst);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateGst = catchAsync(async (req, res) => {
  const methodName = '/updateGst';
  try {
    const gst = await gstService.updateGst(req.body);
    res.send(gst);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllGst = catchAsync(async (req, res) => {
  const methodName = '/getAllGst';
  try {
    const gst = await gstService.getAllGst();
    res.send(gst);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByGstId = catchAsync(async (req, res) => {
  const methodName = '/getByGstId';
  try {
    const gst = await gstService.getById(req.params.gst_id);
    res.send(gst);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByGstId = catchAsync(async (req, res) => {
  const methodName = '/deleteByGstId';
  try {
    const gst = await gstService.deleteGst(req.params.gst_id);
    res.send(gst);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchGst = catchAsync(async (req, res) => {
  const methodName = '/searchGst';
  try {
    const gst = await gstService.searchGst(req.body);
    res.send(gst);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createGst,
  updateGst,
  getAllGst,
  getByGstId,
  deleteByGstId,
  searchGst,
};
