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
      result = {
        message: 'sub_category_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const checkDuplicate =
      await subSubCategoryDao.getBySubSubCategoryNameAndSubCategoryId(
        name,
        Number(sub_category_id)
      );
    if (checkDuplicate) {
      result = {
        message: 'sub_sub_category_name already exist for this sub_category',
        status: false,
        data: null,
      };
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
      result = {
        message: 'sub_category_id does does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const subSubCategoryExist = await subSubCategoryDao.getById(
      sub_sub_category_id
    );
    if (!subSubCategoryExist) {
      result = {
        message: 'sub_sub_category_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const checkDuplicate =
      await subSubCategoryDao.getBySubSubCategoryNameAndSubCategoryId(
        name,
        Number(sub_category_id)
      );
    if (
      checkDuplicate &&
      checkDuplicate?.sub_sub_category_id !== sub_sub_category_id
    ) {
      result = {
        message: 'sub_sub_category_name already exist for this sub_category',
        status: false,
        data: null,
      };
      return result;
    }

    const subSubCategoryDetails = await subSubCategoryDao.edit(
      name,
      sub_category_id,
      budget,
      updated_by,
      sub_sub_category_id
    );
    result = { message: 'success', status: true, data: subSubCategoryDetails };
    return result;
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
      result = { message: 'success', status: true, data: subSubCategoryData };
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
    const subSubCategoryData = {
      message: 'success',
      status: true,
      data: result,
    };
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
        status: false,
        message: 'sub_sub_category_id does Not Exist',
        data: null,
      };
      return result;
    }
    const data = await subSubCategoryDao.deleteSubSubCategory(subSubCategoryId);
    if (data) {
      const result = {
        status: true,
        message: 'SubSubCategory Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this subSubCategory',
        data: null,
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

/**
 * Method to Get All In Active SubSubCategory's
 * @returns
 */
const getAllInActiveSubSubCategories = async () => {
  try {
    const result = await subSubCategoryDao.getAllInActiveSubSubCategories();
    const subSubCategoryData = {
      message: 'success',
      status: true,
      data: result,
    };
    return subSubCategoryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllInActiveSubSubCategories subSubCategory service : ',
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
  getAllInActiveSubSubCategories,
};
