import uomDao from '../dao/uom.dao';
import { createUomBody, updateUomBody } from '../interfaces/uom.Interface';
import itemDao from '../dao/item.dao';

/**
 * Method to Create a New Uom
 * @param body
 * @returns
 */
const createUom = async (body: createUomBody) => {
  try {
    const { name, description, created_by = null } = body;
    const uomDetails = await uomDao.add(name, description, created_by);
    const result = { success: true, data: uomDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in uom service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Uom
 * @param body
 * @returns
 */
const updateUom = async (body: updateUomBody) => {
  try {
    const { name, description, updated_by, uom_id } = body;
    let result = null;
    const uomExist = await uomDao.getById(uom_id);
    if (uomExist) {
      const uomDetails = await uomDao.edit(
        name,
        description,
        updated_by,
        uom_id
      );
      result = { success: true, data: uomDetails };
      return result;
    } else {
      result = { success: false, message: 'uom_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in uom service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Uom By UomId
 * @param uomId
 * @returns
 */
const getById = async (uomId: number) => {
  try {
    let result = null;
    const uomData = await uomDao.getById(uomId);
    if (uomData) {
      result = { success: true, data: uomData };
      return result;
    } else {
      result = { success: false, message: 'uom_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById uom service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Uom's
 * @returns
 */
const getAllUom = async () => {
  try {
    const result = await uomDao.getAll();
    const uomData = { success: true, data: result };
    return uomData;
  } catch (error) {
    console.log('Error occurred in getAllUom uom service : ', error);
    throw error;
  }
};

/**
 * Method to delete uom
 * @param uomId
 */
const deleteUom = async (uomId: number) => {
  try {
    const uomExist = await uomDao.getById(uomId);
    if (!uomExist) {
      const result = {
        status: false,
        message: 'uom_id does not exist',
        data: null,
      };
      return result;
    }
    const uomExistInItem = await itemDao.getByUOMId(uomId);
    if (uomExistInItem) {
      const result = {
        status: false,
        message: 'Unable to delete.This uom_id is mapped in Item Table',
        data: null,
      };
      return result;
    }
    const data = await uomDao.deleteUom(uomId);
    if (data) {
      const result = {
        status: true,
        message: 'Uom Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this uom',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteUom uom service : ', error);
    throw error;
  }
};

/**
 * Method to get Uom By Name
 * @param name
 * @returns
 */
const getByName = async (name: string) => {
  try {
    let result = null;
    const uomData = await uomDao.getByName(name);
    if (uomData.length > 0) {
      result = { success: true, is_exist: true, data: uomData };
      return result;
    } else {
      result = { success: false, is_exist: false };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByName uom service : ', error);
    throw error;
  }
};

export { createUom, updateUom, getAllUom, getById, deleteUom, getByName };
