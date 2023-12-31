import catchAsync from '../utils/catchAsync';
import * as masterDataService from '../services/masterData.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createMasterData = catchAsync(async (req, res) => {
  const methodName = '/createMasterData';
  try {
    const masterData = await masterDataService.createMasterData(req.body);
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateMasterData = catchAsync(async (req, res) => {
  const methodName = '/updateMasterData';
  try {
    const masterData = await masterDataService.updateMasterData(req.body);
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllMasterData = catchAsync(async (req, res) => {
  const methodName = '/getAllMasterData';
  try {
    const masterData = await masterDataService.getAllMasterData();
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByMasterDataId = catchAsync(async (req, res) => {
  const methodName = '/getByMasterDataId';
  try {
    const masterData = await masterDataService.getById(
      req.params.master_data_id
    );
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByMasterDataId = catchAsync(async (req, res) => {
  const methodName = '/deleteByMasterDataId';
  try {
    const masterData = await masterDataService.deleteMasterData(
      req.params.master_data_id
    );
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByParentMasterDataType = catchAsync(async (req, res) => {
  const methodName = '/getByParentMasterDataType';
  try {
    const masterData = await masterDataService.getByParentMasterDataType(
      req.body
    );
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllParentMasterData = catchAsync(async (req, res) => {
  const methodName = '/getAllParentMasterData';
  try {
    const masterData = await masterDataService.getAllParentMasterData();
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const searchMasterData = catchAsync(async (req, res) => {
  const methodName = '/searchMasterData';
  try {
    const masterData = await masterDataService.searchMasterData(req.body);
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByParentType = catchAsync(async (req, res) => {
  const methodName = '/getByParentType';
  try {
    const masterData = await masterDataService.getByParentType(
      req.params.master_data_type
    );
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectId = catchAsync(async (req, res) => {
  const methodName = '/getByProjectId';
  try {
    const masterData = await masterDataService.getByProjectId(
      req.params.project_id
    );
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllProjectMasterData = catchAsync(async (req, res) => {
  const methodName = '/getAllProjectMasterData';
  try {
    const masterData = await masterDataService.getAllProjectMasterData();
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByProjectIdAndType = catchAsync(async (req, res) => {
  const methodName = '/getByProjectIdAndType';
  try {
    const masterData = await masterDataService.getByProjectIdAndType(
      req.params.project_id,
      req.params.master_data_type
    );
    res.send(masterData);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createMasterData,
  updateMasterData,
  getAllMasterData,
  getByMasterDataId,
  deleteByMasterDataId,
  getByParentMasterDataType,
  getAllParentMasterData,
  searchMasterData,
  getByParentType,
  getByProjectId,
  getAllProjectMasterData,
  getByProjectIdAndType,
};
