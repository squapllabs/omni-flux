import uomDao from '../dao/uom.dao';
import { createUomBody, updateUomBody } from '../interfaces/uom.Interface';
import itemDao from '../dao/item.dao';
import projectWorkbreakDownDao from '../dao/projectWorkbreakDown.dao';
import bomDao from '../dao/bom.dao';

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

    const uomExistInProjectWorkbreakDown =
      await projectWorkbreakDownDao.getByUomId(uomId);
    if (uomExistInProjectWorkbreakDown) {
      const result = {
        status: false,
        message:
          'Unable to delete.This uom_id is mapped in Project Workbreak Down Table',
        data: null,
      };
      return result;
    }

    const uomExistInBom = await bomDao.getByUomId(uomId);
    if (uomExistInBom) {
      const result = {
        status: false,
        message: 'Unable to delete.This uom_id is mapped in BOM Table',
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

/**
 * Method to search Uom - Pagination API
 * @returns
 */
const searchUom = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const filterObj = {
      filterUom: {
        AND: [],
        OR: [
          { name: { contains: global_search, mode: 'insensitive' } },
          { description: { contains: global_search, mode: 'insensitive' } },
        ],
      },
    };

    const result = await uomDao.searchUOM(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempUomData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempUomData;
  } catch (error) {
    console.log('Error occurred in Uom service : searchUom', error);
    throw error;
  }
};

export {
  createUom,
  updateUom,
  getAllUom,
  getById,
  deleteUom,
  getByName,
  searchUom,
};
