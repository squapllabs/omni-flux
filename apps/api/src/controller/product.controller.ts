import catchAsync from '../utils/catchAsync';
import * as productService from '../services/product.service';
import { handleError, ErrorHandler } from '../config/error';

const errorText = 'Error';
const createProduct = catchAsync(async (req, res) => {
    const methodName = '/createProduct';
    try {
      const result = await productService.createProduct(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const addBulkProduct = catchAsync(async (req, res) => {
    const methodName = '/addBulkProduct';
    console.log(req.body)
    try {
      const result = await productService.createProductBulk(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getAllProduct = catchAsync(async (req, res) => {
    const methodName = '/getAllProduct';
    try {
      const product = await productService.getAllProduct();
      res.send(product);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const getByProductId = catchAsync(async (req, res) => {
    const methodName = '/getByProductId';
    try {
      const product = await productService.getById(req.params.product_id);
      res.send(product);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const deleteByProductId = catchAsync(async (req, res) => {
    const methodName = '/deleteByProductId';
    try {
      const product = await productService.deleteProduct(req.params.product_id);
      res.send(product);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  const updateProduct = catchAsync(async (req, res) => {
    const methodName = '/updateProduct';
    try {
      const product = await productService.updateProduct(req.body);
      res.send(product);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  });
  export {
    createProduct,
    getAllProduct,
    getByProductId,
    deleteByProductId,
    updateProduct,
    addBulkProduct
  }