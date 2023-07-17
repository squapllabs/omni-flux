import catchAsync from '../utils/catchAsync';
import * as clientService from '../services/client.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createClient = catchAsync(async (req, res) => {
  const methodName = '/createClient';
  try {
    const client = await clientService.createClient(req.body);
    res.send(client);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const updateClient = catchAsync(async (req, res) => {
  const methodName = '/updateClient';
  try {
    const client = await clientService.updateClient(req.body);
    res.send(client);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getAllClients = catchAsync(async (req, res) => {
  const methodName = '/getAllClients';
  try {
    const client = await clientService.getAllClients();
    res.send(client);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const getByClientId = catchAsync(async (req, res) => {
  const methodName = '/getByClientId';
  try {
    const client = await clientService.getById(req.params.client_id);
    res.send(client);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

const deleteByClientId = catchAsync(async (req, res) => {
  const methodName = '/deleteByClientId';
  try {
    const client = await clientService.deleteClient(req.params.client_id);
    res.send(client);
  } catch (err) {
    handleError(new ErrorHandler(errorText, methodName, err), res);
  }
});

export {
  createClient,
  updateClient,
  getAllClients,
  getByClientId,
  deleteByClientId,
};
