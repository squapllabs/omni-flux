import masterDataDao from '../dao/masterData.dao';
import {
  createMasterDataBody,
  updateMasterDataBody,
} from '../interfaces/masterData.Interface';

/**
 * Method to Create a New MasterData
 * @param body
 * @returns
 */
const createMasterData = async (body: createMasterDataBody) => {
  try {
    const {
      master_data_name,
      master_data_description,
      master_data_type,
      parent_master_data_id,
      created_by,
    } = body;

    let result = null;
    if (parent_master_data_id) {
      const parentMasterDataExist = await masterDataDao.getById(
        parent_master_data_id
      );
      if (!parentMasterDataExist) {
        result = {
          message: 'parent_master_data_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }
    const checkDuplicateParentType =
      await masterDataDao.getByParentMasterDataIdAndType(
        parent_master_data_id,
        master_data_type
      );
    if (checkDuplicateParentType) {
      result = {
        message: 'This master_data_type is already exist!',
        status: false,
        data: null,
      };
      return result;
    }
    const masterDataDetails = await masterDataDao.add(
      master_data_name,
      master_data_description,
      master_data_type,
      parent_master_data_id,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: masterDataDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in masterData service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing MasterData
 * @param body
 * @returns
 */
const updateMasterData = async (body: updateMasterDataBody) => {
  try {
    const {
      master_data_name,
      master_data_description,
      master_data_type,
      updated_by,
      master_data_id,
    } = body;
    let result = null;

    const masterDataExist = await masterDataDao.getById(master_data_id);
    if (masterDataExist) {
      const masterDataDetails = await masterDataDao.edit(
        master_data_name,
        master_data_description,
        master_data_type,
        updated_by,
        master_data_id
      );
      result = {
        message: 'success',
        status: true,
        data: masterDataDetails,
      };
      return result;
    } else {
      result = {
        message: 'master_data_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in masterData service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get MasterData By MasterDataId
 * @param masterDataId
 * @returns
 */
const getById = async (masterDataId: number) => {
  try {
    let result = null;
    const masterDataDetails = await masterDataDao.getById(masterDataId);
    if (masterDataDetails) {
      result = {
        message: 'success',
        status: true,
        data: masterDataDetails,
      };
      return result;
    } else {
      result = {
        message: 'master_data_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById masterData service : ', error);
    throw error;
  }
};

/**
 * Method to Get All MasterData's
 * @returns
 */
const getAllMasterData = async () => {
  try {
    const result = await masterDataDao.getAll();
    const masterDataDetails = {
      message: 'success',
      status: true,
      data: result,
    };
    return masterDataDetails;
  } catch (error) {
    console.log(
      'Error occurred in getAllMasterData masterData service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All getAllParentMasterData's
 * @returns
 */
const getAllParentMasterData = async () => {
  try {
    const result = await masterDataDao.getAllParentMasterData();
    const masterDataDetails = {
      message: 'success',
      status: true,
      data: result,
    };
    return masterDataDetails;
  } catch (error) {
    console.log(
      'Error occurred in getAllParentMasterData masterData service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete masterData
 * @param masterDataId
 */
const deleteMasterData = async (masterDataId: number) => {
  try {
    const masterDataExist = await masterDataDao.getById(masterDataId);
    if (!masterDataExist) {
      const result = {
        message: 'master_data_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const masterDataIdAsParent = await masterDataDao.getByParentMasterDataId(
      masterDataId
    );
    if (masterDataIdAsParent) {
      const result = {
        message: 'Unable to delete.This is mapped in parent_master_data_id',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await masterDataDao.deleteMasterData(masterDataId);
    if (data) {
      const result = {
        message: 'MasterData Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this masterData',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteMasterData masterData service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get MasterData By parentMasterDataType
 * @param parentMasterDataType
 * @returns
 */
const getByParentMasterDataType = async (parentMasterDataType: string) => {
  try {
    let result = null;
    const masterDataDetails = await masterDataDao.getByParentMasterDataType(
      parentMasterDataType
    );
    if (masterDataDetails) {
      result = { status: true, is_exist: true, data: masterDataDetails };
      return result;
    } else {
      result = { status: false, is_exist: false, data: null };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByParentMasterDataType masterData service : ',
      error
    );
    throw error;
  }
};

export {
  createMasterData,
  updateMasterData,
  getAllMasterData,
  getAllParentMasterData,
  getById,
  deleteMasterData,
  getByParentMasterDataType,
};
