import subCategoryDao from '../dao/subCategory.dao';
import categoryDao from '../dao/category.dao';
import {
  createSubCategoryBody,
  updateSubCategoryBody,
} from '../interfaces/subCategory.Interface';

/**
 * Method to Create a New SubCategory
 * @param body
 * @returns
 */
const createSubCategory = async (body: createSubCategoryBody) => {
  try {
    const { name, category_id, budget, created_by = null } = body;
    let result = null;
    const categoryExist = await categoryDao.getById(category_id);
    if (!categoryExist) {
      result = { success: false, message: 'category_id does not exist' };
      return result;
    }
    const subCategoryDetails = await subCategoryDao.add(
      name,
      category_id,
      budget,
      created_by
    );
    result = { success: true, data: subCategoryDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in subCategory service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing SubCategory
 * @param body
 * @returns
 */
const updateSubCategory = async (body: updateSubCategoryBody) => {
  try {
    const { name, category_id, budget, updated_by, sub_category_id } = body;
    let result = null;
    const categoryExist = await categoryDao.getById(category_id);
    if (!categoryExist) {
      result = { success: false, message: 'category_id does not exist' };
      return result;
    }
    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (subCategoryExist) {
      const subCategoryDetails = await subCategoryDao.edit(
        name,
        category_id,
        budget,
        updated_by,
        sub_category_id
      );
      result = { success: true, data: subCategoryDetails };
      return result;
    } else {
      result = { success: false, message: 'sub_category_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in subCategory service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get SubCategory By subCategoryId
 * @param subCategoryId
 * @returns
 */
const getById = async (subCategoryId: number) => {
  try {
    let result = null;
    const subCategoryData = await subCategoryDao.getById(subCategoryId);
    if (subCategoryData) {
      result = { success: true, data: subCategoryData };
      return result;
    } else {
      result = { success: false, message: 'sub_category_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById subCategory service : ', error);
    throw error;
  }
};

/**
 * Method to Get All SubCategory's
 * @returns
 */
const getAllSubCategory = async () => {
  try {
    const result = await subCategoryDao.getAll();
    const subCategoryData = { success: true, data: result };
    return subCategoryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllSubCategory subCategory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete subCategory
 * @param subCategoryId
 */
const deleteSubCategory = async (subCategoryId: number) => {
  try {
    const subCategoryExist = await subCategoryDao.getById(subCategoryId);
    if (!subCategoryExist) {
      const result = {
        success: false,
        message: 'sub_category_id does Not Exist',
      };
      return result;
    }
    const data = await subCategoryDao.deleteSubCategory(subCategoryId);
    if (data) {
      const result = {
        success: true,
        message: 'SubCategory Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this subCategory',
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteSubCategory subCategory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to check Duplicate Sub Category Name by sub_category_name and category_id
 * @param subCategoryName
 * @param categoryId
 * @returns
 */
const checkDuplicateSubCategoryName = async (
  subCategoryName: string,
  categoryId: number
) => {
  try {
    let result = null;
    const subCategoryData =
      await subCategoryDao.getBySubCategoryNameAndCategoryId(
        subCategoryName,
        Number(categoryId)
      );
    if (subCategoryData) {
      result = {
        message: 'success',
        status: true,
        is_exist: true,
        data: subCategoryData,
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
      'Error occurred in checkDuplicateSubCategoryName subCategory service : ',
      error
    );
    throw error;
  }
};

export {
  createSubCategory,
  updateSubCategory,
  getAllSubCategory,
  getById,
  deleteSubCategory,
  checkDuplicateSubCategoryName,
};
