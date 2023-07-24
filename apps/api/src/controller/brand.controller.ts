import catchAsync from '../utils/catchAsync';
import * as brandService from '../services/brand.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';
const addBrand = catchAsync(async (req, res) => {
    const methodName = '/addBrand';
    try {
      const result = await brandService.addBrand(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getAllBrand = catchAsync(async (req, res) => {
    const methodName = '/getAllBrand';
    try {
      const brand = await brandService.getAllBrand();
      res.send(brand);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getByBrandId = catchAsync(async (req, res) => {
    const methodName = '/getByBrandId';
    try {
      const brand = await brandService.getById(req.params.brand_id);
      res.send(brand);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const deleteByBrandId = catchAsync(async (req, res) => {
    const methodName = '/deleteByBrandId';
    try {
      const brand = await brandService.deleteBrand(req.params.brand_id);
      res.send(brand);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const updateBrand = catchAsync(async (req, res) => {
    const methodName = '/updateBrand';
    try {
      const brand = await brandService.updateBrand(req.body);
      res.send(brand);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  export{
    addBrand,
    getAllBrand,
    getByBrandId,
    deleteByBrandId,
    updateBrand
  }