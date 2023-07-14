import hsnCodeDao from '../dao/hsnCode.dao';
import {
  createHsnCodeBody,
  updateHsnCodeBody,
} from '../interfaces/hsnCode.Interface';

/**
 * Method to Create a New HsnCode
 * @param body
 * @returns
 */
const createHsnCode = async (body: createHsnCodeBody) => {
  try {
    const { code, description, created_by = null } = body;
    const hsnCodeDetails = await hsnCodeDao.add(code, description, created_by);
    const result = { success: true, data: hsnCodeDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in hsnCode service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing HsnCode
 * @param body
 * @returns
 */
const updateHsnCode = async (body: updateHsnCodeBody) => {
  try {
    const { code, description, updated_by, hsn_code_id } = body;
    let result = null;
    const hsnCodeExist = await hsnCodeDao.getById(hsn_code_id);
    if (hsnCodeExist) {
      const hsnCodeDetails = await hsnCodeDao.edit(
        code,
        description,
        updated_by,
        hsn_code_id
      );
      result = { success: true, data: hsnCodeDetails };
      return result;
    } else {
      result = { success: false, message: 'hsn_code_id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in hsnCode service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get HsnCode By HsnCodeId
 * @param hsnCodeId
 * @returns
 */
const getById = async (hsnCodeId: number) => {
  try {
    let result = null;
    const hsnCodeData = await hsnCodeDao.getById(hsnCodeId);
    if (hsnCodeData) {
      result = { success: true, data: hsnCodeData };
      return result;
    } else {
      result = { success: false, message: 'hsn_code_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById hsnCode service : ', error);
    throw error;
  }
};

/**
 * Method to Get All HsnCode's
 * @returns
 */
const getAllHsnCode = async () => {
  try {
    const result = await hsnCodeDao.getAll();
    const hsnCodeData = { success: true, data: result };
    return hsnCodeData;
  } catch (error) {
    console.log('Error occurred in getAllHsnCode hsnCode service : ', error);
    throw error;
  }
};

/**
 * Method to delete hsnCode
 * @param hsnCodeId
 */
const deleteHsnCode = async (hsnCodeId: number) => {
  try {
    const hsnCodeExist = await hsnCodeDao.getById(hsnCodeId);
    if (!hsnCodeExist) {
      const result = { success: false, message: 'HsnCode Id Not Exist' };
      return result;
    }
    const data = await hsnCodeDao.deleteHsnCode(hsnCodeId);
    if (data) {
      const result = {
        success: true,
        message: 'HsnCode Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this hsnCode',
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteHsnCode hsnCode service : ', error);
    throw error;
  }
};

export { createHsnCode, updateHsnCode, getAllHsnCode, getById, deleteHsnCode };
