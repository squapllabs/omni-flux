import uomDao from '../dao/uom.dao';
import { createUomBody, updateUomBody } from '../interfaces/uom.Interface';
import itemDao from '../dao/item.dao';
import projectWorkbreakDownDao from '../dao/projectWorkbreakDown.dao';
import bomDao from '../dao/bomDetail.dao';

/**
 * Method to Create a New Uom
 * @param body
 * @returns
 */
const createUom = async (body: createUomBody) => {
  try {
    const { name, description, created_by = null, uom_type } = body;
    const uomDetails = await uomDao.add(
      name,
      description,
      created_by,
      uom_type
    );
    const result = { message: 'success', status: true, data: uomDetails };
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
    const { name, description, updated_by, uom_type, uom_id } = body;
    let result = null;
    const uomExist = await uomDao.getById(uom_id);
    if (uomExist) {
      const uomDetails = await uomDao.edit(
        name,
        description,
        updated_by,
        uom_type,
        uom_id
      );
      result = { message: 'success', status: true, data: uomDetails };
      return result;
    } else {
      result = { message: 'uom_id does not exist', status: false, data: null };
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
      result = { message: 'success', status: true, data: uomData };
      return result;
    } else {
      result = { message: 'uom_id does not exist', status: false, data: null };
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
    const uomData = { message: 'success', status: true, data: result };
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
        message: 'uom_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const uomExistInItem = await itemDao.getByUOMId(uomId);
    if (uomExistInItem) {
      const result = {
        message: 'Unable to delete.This uom_id is mapped in Item Table',
        status: false,
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
        message: 'Uom Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this uom',
        status: false,
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
      result = {
        message: 'success',
        status: true,
        is_exist: true,
        data: uomData,
      };
      return result;
    } else {
      result = {
        message: 'name is not exist',
        status: false,
        data: null,
        is_exist: false,
      };
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
    if (result.count > 0) {
      const tempUomData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempUomData;
    } else if (result.count === 0) {
      const tempUomData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempUomData;
    } else {
      const tempUomData = {
        message: 'fail',
        status: true,
        is_available: false,
      };
      return tempUomData;
    }
  } catch (error) {
    console.log('Error occurred in Uom service : searchUom', error);
    throw error;
  }
};

/**
 * Method to get Uom By Uom Type
 * @param uomType
 * @returns
 */
const getByType = async (uomType: string) => {
  try {
    let result = null;
    const uomData = await uomDao.getByType(uomType);
    if (uomData.length > 0) {
      result = { message: 'success', status: true, data: uomData };
      return result;
    } else {
      result = {
        message: 'uom does not exist for this uom_type',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByType uom service : ', error);
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
  getByType,
};
