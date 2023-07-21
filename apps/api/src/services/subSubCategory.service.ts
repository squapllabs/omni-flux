import subSubCategoryDao from '../dao/subSubCategory.dao';
import subCategoryDao from '../dao/subCategory.dao';
import {
  createSubSubCategoryBody,
  updateSubSubCategoryBody,
} from '../interfaces/subSubCategory.Interface';

/**
 * Method to Create a New SubSubCategory
 * @param body
 * @returns
 */
const createSubSubCategory = async (body: createSubSubCategoryBody) => {
  try {
    const { name, sub_category_id, budget, created_by = null } = body;
    let result = null;
    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      result = { success: false, message: 'sub_category_id does not exist' };
      return result;
    }
    const subSubCategoryDetails = await subSubCategoryDao.add(
      name,
      sub_category_id,
      budget,
      created_by
    );
    result = { success: true, data: subSubCategoryDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in subSubCategory service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing SubSubCategory
 * @param body
 * @returns
 */
const updateSubSubCategory = async (body: updateSubSubCategoryBody) => {
  try {
    const { name, sub_category_id, budget, updated_by, sub_sub_category_id } =
      body;
    let result = null;
    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      result = { success: false, message: 'sub_category_id does not exist' };
      return result;
    }

    const subSubCategoryExist = await subSubCategoryDao.getById(
      sub_sub_category_id
    );
    if (subSubCategoryExist) {
      const subSubCategoryDetails = await subSubCategoryDao.edit(
        name,
        sub_category_id,
        budget,
        updated_by,
        sub_sub_category_id
      );
      result = { success: true, data: subSubCategoryDetails };
      return result;
    } else {
      result = { success: false, message: 'sub_sub_category_id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in subSubCategory service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get SubSubCategory By subSubCategoryId
 * @param subSubCategoryId
 * @returns
 */
const getById = async (subSubCategoryId: number) => {
  try {
    let result = null;
    const subSubCategoryData = await subSubCategoryDao.getById(
      subSubCategoryId
    );
    if (subSubCategoryData) {
      result = { success: true, data: subSubCategoryData };
      return result;
    } else {
      result = {
        success: false,
        message: 'sub_sub_category_id does not exist',
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById subSubCategory service : ', error);
    throw error;
  }
};

/**
 * Method to Get All SubSubCategory's
 * @returns
 */
const getAllSubSubCategory = async () => {
  try {
    const result = await subSubCategoryDao.getAll();
    const subSubCategoryData = { success: true, data: result };
    return subSubCategoryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllSubSubCategory subSubCategory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete subSubCategory
 * @param subSubCategoryId
 */
const deleteSubSubCategory = async (subSubCategoryId: number) => {
  try {
    const subSubCategoryExist = await subSubCategoryDao.getById(
      subSubCategoryId
    );
    if (!subSubCategoryExist) {
      const result = {
        success: false,
        message: 'sub_sub_category_id does Not Exist',
      };
      return result;
    }
    const data = await subSubCategoryDao.deleteSubSubCategory(subSubCategoryId);
    if (data) {
      const result = {
        success: true,
        message: 'SubSubCategory Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this subSubCategory',
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteSubSubCategory subSubCategory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to check Duplicate Sub Sub Category Name by sub_sub_category_name and sub_category_id
 * @param subSubCategoryName
 * @param subCategoryId
 * @returns
 */
const checkDuplicateSubSubCategoryName = async (
  subSubCategoryName: string,
  subCategoryId: number
) => {
  try {
    let result = null;
    const subSubCategoryData =
      await subSubCategoryDao.getBySubSubCategoryNameAndSubCategoryId(
        subSubCategoryName,
        Number(subCategoryId)
      );
    if (subSubCategoryData) {
      result = {
        message: 'success',
        status: true,
        is_exist: true,
        data: subSubCategoryData,
      };
      return result;
    } else {
      result = {
        message: 'failed',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in checkDuplicateSubSubCategoryName subSubCategory service : ',
      error
    );
    throw error;
  }
};

export {
  createSubSubCategory,
  updateSubSubCategory,
  getAllSubSubCategory,
  getById,
  deleteSubSubCategory,
  checkDuplicateSubSubCategoryName,
};
