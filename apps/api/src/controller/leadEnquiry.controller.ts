import catchAsync from '../utils/catchAsync';
import * as leadEnquiryService from '../services/leadEnquiry.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createLeadEnquiry = catchAsync(async (req, res) => {
  const methodName = '/createLeadEnquiry';
  try {
    const leadEnquiry = await leadEnquiryService.createLeadEnquiry(req.body);
    res.send(leadEnquiry);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateLeadEnquiry = catchAsync(async (req, res) => {
  const methodName = '/updateLeadEnquiry';
  try {
    const leadEnquiry = await leadEnquiryService.updateLeadEnquiry(req.body);
    res.send(leadEnquiry);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllLeadEnquiry = catchAsync(async (req, res) => {
  const methodName = '/getAllLeadEnquiry';
  try {
    const leadEnquiry = await leadEnquiryService.getAllLeadEnquiry();
    res.send(leadEnquiry);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByLeadEnquiryId = catchAsync(async (req, res) => {
  const methodName = '/getByLeadEnquiryId';
  try {
    const leadEnquiry = await leadEnquiryService.getById(
      req.params.lead_enquiry_id
    );
    res.send(leadEnquiry);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByLeadEnquiryId = catchAsync(async (req, res) => {
  const methodName = '/deleteByLeadEnquiryId';
  try {
    const leadEnquiry = await leadEnquiryService.deleteLeadEnquiry(
      req.params.lead_enquiry_id
    );
    res.send(leadEnquiry);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createLeadEnquiry,
  updateLeadEnquiry,
  getAllLeadEnquiry,
  getByLeadEnquiryId,
  deleteByLeadEnquiryId,
};
