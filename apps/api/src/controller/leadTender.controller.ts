import catchAsync from '../utils/catchAsync';
import * as leadTenderService from '../services/leadTender.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createLeadTender = catchAsync(async (req, res) => {
  const methodName = '/createLeadTender';
  try {
    const leadTender = await leadTenderService.createLeadTender(req.body);
    res.send(leadTender);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateLeadTender = catchAsync(async (req, res) => {
  const methodName = '/updateLeadTender';
  try {
    const leadTender = await leadTenderService.updateLeadTender(req.body);
    res.send(leadTender);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllLeadTender = catchAsync(async (req, res) => {
  const methodName = '/getAllLeadTender';
  try {
    const leadTender = await leadTenderService.getAllLeadTender();
    res.send(leadTender);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByLeadTenderId = catchAsync(async (req, res) => {
  const methodName = '/getByLeadTenderId';
  try {
    const leadTender = await leadTenderService.getById(
      req.params.lead_tender_id
    );
    res.send(leadTender);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByLeadTenderId = catchAsync(async (req, res) => {
  const methodName = '/deleteByLeadTenderId';
  try {
    const leadTender = await leadTenderService.deleteLeadTender(
      req.params.lead_tender_id
    );
    res.send(leadTender);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createLeadTender,
  updateLeadTender,
  getAllLeadTender,
  getByLeadTenderId,
  deleteByLeadTenderId,
};
