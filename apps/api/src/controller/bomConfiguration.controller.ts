import catchAsync from '../utils/catchAsync';
import bomConfigurationService from '../services/bomConfiguration.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createBomConfiguration = catchAsync(async (req, res) => {
  const methodName = '/createBomConfiguration';
  try {
    const bomConfiguration =
      await bomConfigurationService.createBomConfiguration(req.body);
    res.send(bomConfiguration);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateBomConfiguration = catchAsync(async (req, res) => {
  const methodName = '/updateBomConfiguration';
  try {
    const bomConfiguration =
      await bomConfigurationService.updateBomConfiguration(req.body);
    res.send(bomConfiguration);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllBomConfiguration = catchAsync(async (req, res) => {
  const methodName = '/getAllBomConfiguration';
  try {
    const bomConfiguration =
      await bomConfigurationService.getAllBomConfiguration();
    res.send(bomConfiguration);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteBomConfiguration = catchAsync(async (req, res) => {
  const methodName = '/deleteBomConfiguration';
  try {
    const bomConfiguration =
      await bomConfigurationService.deleteBomConfiguration(
        req.params.bom_configuration_id
      );
    res.send(bomConfiguration);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByBomConfigurationId = catchAsync(async (req, res) => {
  const methodName = '/getByBomConfigurationId';
  try {
    const bomConfiguration =
      await bomConfigurationService.getByBomConfigurationId(
        req.params.bom_configuration_id
      );
    res.send(bomConfiguration);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchBomConfiguration = catchAsync(async (req, res) => {
  const methodName = '/searchbomconfiguration';
  try {
    const labour = await bomConfigurationService.searchBomConfiguration(
      req.body
    );
    res.send(labour);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createBomConfiguration,
  updateBomConfiguration,
  getAllBomConfiguration,
  deleteBomConfiguration,
  getByBomConfigurationId,
  searchBomConfiguration,
};
