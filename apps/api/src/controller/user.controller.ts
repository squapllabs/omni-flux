/* eslint-disable @typescript-eslint/no-explicit-any */
import catchAsync from '../utils/catchAsync';
import * as userService from '../services/user.service';
import { handleError, ErrorHandler } from '../config/error';
import { Response } from 'express';
const errorText = 'Error';

const createUser = catchAsync(
  async (
    req: {
      body: {
        center_id: Uint8Array;
        username: string;
        userpass: string;
        mobilenumber: string;
        email: string;
        firstname: string;
        lastname: string;
        profileimgurl: string;
        gender: string;
        dob: Date;
        status: string;
        address: string;
        createdby: Uint8Array;
        updatedby: Uint8Array;
      };
    },
    res: Response<any, Record<string, any>>
  ) => {
    const methodName = '/createUser';
    try {
      const result = await userService.createUser(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  }
);

const getByUserId = catchAsync(
  async (
    req: { params: { id: bigint } },
    res: Response<any, Record<string, any>>
  ) => {
    const methodName = '/getById';
    try {
      const result = await userService.getById(req.params.id);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  }
);

const getByEmailId = catchAsync(
  async (req: any, res: Response<unknown, Record<string, any>>) => {
    const methodName = '/getByEmailId';
    try {
      const result = await userService.getByEmailId(req.params.email);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  }
);

const userLogin = catchAsync(
  async (req: any, res: Response<any, Record<string, any>>) => {
    const methodName = '/userLogin';
    try {
      const result = await userService.userLogin(req.body);
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  }
);

const getAllUser = catchAsync(
  async (req: any, res: Response<any, Record<string, any>>) => {
    const methodName = '/getAll';
    try {
      const result = await userService.getAllUser();
      res.send(result);
    } catch (err) {
      handleError(new ErrorHandler(errorText, methodName, err), res);
    }
  }
);

export { createUser, getByUserId, getByEmailId, userLogin, getAllUser };
