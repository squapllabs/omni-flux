import catchAsync from '../utils/catchAsync';
import * as vendorService from '../services/vendor.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createVendor = catchAsync(async (req, res) => {
  const methodName = '/createVendor';
  try {
    const vendor = await vendorService.createVendor(req.body);
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateVendor = catchAsync(async (req, res) => {
  const methodName = '/updateVendor';
  try {
    const vendor = await vendorService.updateVendor(req.body);
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllVendor = catchAsync(async (req, res) => {
  const methodName = '/getAllVendor';
  try {
    const vendor = await vendorService.getAllVendor();
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByVendorId = catchAsync(async (req, res) => {
  const methodName = '/getByVendorId';
  try {
    const vendor = await vendorService.getById(req.params.vendor_id);
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByVendorId = catchAsync(async (req, res) => {
  const methodName = '/deleteByVendorId';
  try {
    const vendor = await vendorService.deleteVendor(req.params.vendor_id);
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchVendor = catchAsync(async (req, res) => {
  const methodName = '/searchVendor';
  try {
    const vendor = await vendorService.searchVendor(req.body);
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByEmailId = catchAsync(async (req, res) => {
  const methodName = '/getByEmailId';
  try {
    const vendor = await vendorService.getByEmailId(req.params.contact_email);
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByVendorName = catchAsync(async (req, res) => {
  const methodName = '/getByVendorName';
  try {
    const vendor = await vendorService.getByVendorName(req.params.vendor_name);
    res.send(vendor);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createVendor,
  updateVendor,
  getAllVendor,
  getByVendorId,
  deleteByVendorId,
  searchVendor,
  getByEmailId,
  getByVendorName,
};
