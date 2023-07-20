import catchAsync from '../utils/catchAsync';
import * as siteService from '../services/site.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createSite = catchAsync(async (req, res) => {
  const methodName = '/createSite';
  try {
    const site = await siteService.createSite(req.body);
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateSite = catchAsync(async (req, res) => {
  const methodName = '/updateSite';
  try {
    const site = await siteService.updateSite(req.body);
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllSites = catchAsync(async (req, res) => {
  const methodName = '/getAllSites';
  try {
    const site = await siteService.getAllSites();
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBySiteId = catchAsync(async (req, res) => {
  const methodName = '/getBySiteId';
  try {
    const site = await siteService.getById(req.params.site_id);
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBySiteId = catchAsync(async (req, res) => {
  const methodName = '/deleteBySiteId';
  try {
    const site = await siteService.deleteSite(req.params.site_id);
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export { createSite, updateSite, getAllSites, getBySiteId, deleteBySiteId };
