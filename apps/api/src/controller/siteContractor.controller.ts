import catchAsync from '../utils/catchAsync';
import * as siteContractorService from '../services/siteContractor.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createSiteContractor = catchAsync(async (req, res) => {
  const methodName = '/createSiteContractor';
  try {
    const site = await siteContractorService.createSiteContractor(req.body);
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateSiteContractor = catchAsync(async (req, res) => {
  const methodName = '/updateSiteContractor';
  try {
    const site = await siteContractorService.updateSiteContractor(req.body);
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllSiteContractors = catchAsync(async (req, res) => {
  const methodName = '/getAllSiteContractors';
  try {
    const site = await siteContractorService.getAllSiteContractors();
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getBySiteContractorId = catchAsync(async (req, res) => {
  const methodName = '/getBySiteContractorId';
  try {
    const site = await siteContractorService.getById(
      req.params.site_contractor_id
    );
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteSiteContractorById = catchAsync(async (req, res) => {
  const methodName = '/deleteSiteContractorById';
  try {
    const site = await siteContractorService.deleteSiteContractor(
      req.params.site_contractor_id
    );
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllSites = catchAsync(async (req, res) => {
  const methodName = '/getAllSites';
  try {
    const site = await siteContractorService.getAllSites();
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllContractors = catchAsync(async (req, res) => {
  const methodName = '/getAllContractors';
  try {
    const site = await siteContractorService.getAllContractors();
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchSiteContractor = catchAsync(async (req, res) => {
  const methodName = '/searchSiteContractor';
  try {
    const site = await siteContractorService.searchSiteContractor(req.body);
    res.send(site);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createSiteContractor,
  updateSiteContractor,
  getAllSiteContractors,
  getBySiteContractorId,
  deleteSiteContractorById,
  getAllSites,
  getAllContractors,
  searchSiteContractor,
};
