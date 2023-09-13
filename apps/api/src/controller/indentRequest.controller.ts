import catchAsync from '../utils/catchAsync';
import * as indentRequestService from '../services/indentRequest.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createIndentRequest = catchAsync(async (req, res) => {
  const methodName = '/createIndentRequest';
  try {
    const indentRequest = await indentRequestService.createIndentRequest(
      req.body
    );
    res.send(indentRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateIndentRequest = catchAsync(async (req, res) => {
  const methodName = '/updateIndentRequest';
  try {
    const indentRequest = await indentRequestService.updateIndentRequest(
      req.body
    );
    res.send(indentRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllIndentRequests = catchAsync(async (req, res) => {
  const methodName = '/getAllIndentRequests';
  try {
    const indentRequest = await indentRequestService.getAllIndentRequests();
    res.send(indentRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByIndentRequestId = catchAsync(async (req, res) => {
  const methodName = '/getByIndentRequestId';
  try {
    const indentRequest = await indentRequestService.getById(
      req.params.indent_request_id
    );
    res.send(indentRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByIndentRequestId = catchAsync(async (req, res) => {
  const methodName = '/deleteByIndentRequestId';
  try {
    const indentRequest = await indentRequestService.deleteIndentRequest(
      req.params.indent_request_id
    );
    res.send(indentRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchIndentRequest = catchAsync(async (req, res) => {
  const methodName = '/searchIndentRequest';
  try {
    const indentRequest = await indentRequestService.searchIndentRequest(
      req.body
    );
    res.send(indentRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const indentRequest = await indentRequestService.getByProjectId(
      req.params.project_id
    );
    res.send(indentRequest);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createIndentRequest,
  updateIndentRequest,
  getAllIndentRequests,
  getByIndentRequestId,
  deleteByIndentRequestId,
  searchIndentRequest,
  getByProjectId,
};
