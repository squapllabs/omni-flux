import uomDao from '../dao/uom.dao';
import { createUomBody, updateUomBody } from '../interfaces/uom.Interface';

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
      result = { success: false, message: 'uom_id not exist' };
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
      result = { success: false, message: 'uom id not exist' };
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
      const result = { success: false, message: 'Uom Id Not Exist' };
      return result;
    }
    const data = await uomDao.deleteUom(uomId);
    if (data) {
      const result = {
        success: true,
        message: 'Uom Data Deleted Successfully',
      };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this uom' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteUom uom service : ', error);
    throw error;
  }
};

export { createUom, updateUom, getAllUom, getById, deleteUom };
