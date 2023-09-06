import catchAsync from '../utils/catchAsync';
import * as MachineryService from '../services/machinery.service';
import { handleError, ErrorHandler } from '../config/error';
const errorText = 'Error';

const createMachinery = catchAsync(async (req, res) => {
    const methodName = '/createMachinery';
    try {
        const machinery = await MachineryService.createMachinery(req.body);
        res.send(machinery);
    } catch (err) {
        handleError(new ErrorHandler(errorText, methodName, err), res);
    }
});

const updateMachinery = catchAsync(async (req, res) => {
    const methodName = '/updateMachinery';
    try {
        const machinery = await MachineryService.updateMachinery(req.body);
        res.send(machinery);
    } catch (err) {
        handleError(new ErrorHandler(errorText, methodName, err), res);
    }
});

const getAllMachinery = catchAsync(async (req, res) => {
    const methodName = '/getAllMachinery';
    try {
        const machinery = await MachineryService.getAllMachinery();
        res.send(machinery);
    } catch (err) {
        handleError(new ErrorHandler(errorText, methodName, err), res);
    }
});

const getByMachineryId = catchAsync(async (req, res) => {
    const methodName = '/getByMachineryId';
    try {
        const machinery = await MachineryService.getById(
            req.params.Machinery_id
        );
        res.send(machinery);
    } catch (err) {
        handleError(new ErrorHandler(errorText, methodName, err), res);
    }
});

const deleteByMachineryId = catchAsync(async (req, res) => {
    const methodName = '/deleteByMachineryId';
    try {
        const machinery = await MachineryService.deleteMachinery(
            req.params.machinery_id
        );
        res.send(machinery);
    } catch (err) {
        handleError(new ErrorHandler(errorText, methodName, err), res);
    }
});

const searchMachinery = catchAsync(async (req, res) => {
    const methodName = '/searchMachinery';
    try {
        const machinery = await MachineryService.searchMachinery(req.body);
        res.send(machinery);
    } catch (err) {
        handleError(new ErrorHandler(errorText, methodName, err), res);
    }
});


export {
    createMachinery,
    updateMachinery,
    getAllMachinery,
    getByMachineryId,
    deleteByMachineryId,
    searchMachinery,
};
