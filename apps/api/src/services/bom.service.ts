import bomDao from '../dao/bom.dao';
import { bomBody } from '../interfaces/bom.interface';

/**
 * Method to Create a New Bom
 * @param body
 * @returns
 */
const createBom = async (body: bomBody) => {
  try {
    let result = null;
    const {
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      created_by,
    } = body;
    const bomData = await bomDao.add(
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      created_by
    );
    if (bomData !== null) {
      result = {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      result = {
        message: 'bom_name already exist for this project',
        status: false,
        data: null,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occured in bom.Service createBom', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Bom
 * @param body
 * @returns
 */
const updateBom = async (body: bomBody) => {
  try {
    const {
      bom_id,
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      is_delete,
      updated_by,
    } = body;
    let result = null;
    const bomExist = await bomDao.getById(bom_id);
    if (!bomExist) {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const bomDetails = await bomDao.edit(
      bom_id,
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      is_delete,
      updated_by
    );
    result = {
      message: 'success',
      status: true,
      data: bomDetails,
    };

    return result;
  } catch (error) {
    console.log('Error occured in updateBom Bom.service', error);
    throw error;
  }
};

/**
 * Method to Get All Bom's
 * @returns
 */
const getAllBom = async () => {
  try {
    const result = await bomDao.getAll();
    const bomData = { message: 'success', status: true, data: result };
    return bomData;
  } catch (error) {
    console.log('Error occurred in getAllBom Bom.service : ', error);
    throw error;
  }
};

/**
 * Method to get Bom By BomId
 * @param bomId
 * @returns
 */
const fetchBomById = async (bom_id: number) => {
  try {
    let result = null;
    const bomData = await bomDao.getById(bom_id);
    if (bomData) {
      result = {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occurred in bom_id', error);
    throw error;
  }
};

/**
 * Method to delete bom
 * @param bomId
 */
const deleteBomById = async (bom_id: number) => {
  try {
    let result = null;
    const bomExist = await bomDao.getById(bom_id);
    if (!bomExist) {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
    } else {
      const data = await bomDao.deleteBom(bom_id);
      if (data) {
        result = {
          message: 'Bom Data Successfully Deleted',
          status: true,
          data: null,
        };
      } else {
        result = {
          message: 'Failed to delete this bom',
          status: false,
          data: null,
        };
      }
    }
    return result;
  } catch (error) {
    console.log('Error occurred in delete bom.service', error);
    throw error;
  }
};

/**
 * Method to get BOM By Category Id Sub Category ID And Sub Sub Category Id
 * @param body
 * @returns
 */
const getByCategorySubCatAndSubSubCatId = async (body) => {
  try {
    let result = null;
    const {
      category_id,
      sub_category_id = null,
      sub_sub_category_id = null,
    } = body;
    const bomData = await bomDao.getByCategorySubCatAndSubSubCatId(
      category_id,
      sub_category_id,
      sub_sub_category_id
    );
    result = { message: 'success', status: true, data: bomData };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in getByCategorySubCatAndSubSubCatId bom service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to entireData bom
 * @param bomId
 */
const getEntireDataByBomId = async (bom_id: number) => {
  try {
    let result = null;
    const bomExist = await bomDao.getById(bom_id);
    if (!bomExist) {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
    } else {
      const bomEntireData = await bomDao.entireData(bom_id);
      result = {
        message: 'success',
        status: true,
        data: bomEntireData,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occurred in getEntireDataByBomId bom.service', error);
    throw error;
  }
};

export {
  deleteBomById,
  createBom,
  updateBom,
  fetchBomById,
  getAllBom,
  getByCategorySubCatAndSubSubCatId,
  getEntireDataByBomId,
};
