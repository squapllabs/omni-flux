import catchAsync from '../utils/catchAsync';
import * as capabilityService from '../services/capability.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createCapability = catchAsync(async (req, res) => {
  const methodName = '/createCapability';
  try {
    const capability = await capabilityService.createCapability(req.body);
    res.send(capability);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateCapability = catchAsync(async (req, res) => {
  const methodName = '/updateCapability';
  try {
    const capability = await capabilityService.updateCapability(req.body);
    res.send(capability);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllCapabilities = catchAsync(async (req, res) => {
  const methodName = '/getAllCapabilities';
  try {
    const capability = await capabilityService.getAllCapabilities();
    res.send(capability);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByCapabilityId = catchAsync(async (req, res) => {
  const methodName = '/getByCapabilityId';
  try {
    const capability = await capabilityService.getById(
      req.params.capability_id
    );
    res.send(capability);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByCapabilityId = catchAsync(async (req, res) => {
  const methodName = '/deleteByCapabilityId';
  try {
    const capability = await capabilityService.deleteCapability(
      req.params.capability_id
    );
    res.send(capability);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchCapability = catchAsync(async (req, res) => {
  const methodName = '/searchCapability';
  try {
    const capability = await capabilityService.searchCapability(req.body);
    res.send(capability);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createCapability,
  updateCapability,
  getAllCapabilities,
  getByCapabilityId,
  deleteByCapabilityId,
  searchCapability,
};
