import subCategoryDao from '../dao/subCategory.dao';
import categoryDao from '../dao/category.dao';
import {
  createSubCategoryBody,
  updateSubCategoryBody,
} from '../interfaces/subCategory.Interface';
import subSubCategoryDao from '../dao/subSubCategory.dao';

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
    const checkDuplicate =
      await subCategoryDao.getBySubCategoryNameAndCategoryId(
        name,
        Number(category_id)
      );
    if (checkDuplicate) {
      result = {
        message: 'sub_category_name already exist for this category',
        status: false,
        data: null,
      };
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
    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      result = { success: false, message: 'sub_category_id does not exist' };
      return result;
    }
    const categoryExist = await categoryDao.getById(category_id);
    if (!categoryExist) {
      result = { success: false, message: 'category_id does not exist' };
      return result;
    }
    const checkDuplicate =
      await subCategoryDao.getBySubCategoryNameAndCategoryId(
        name,
        Number(category_id)
      );
    if (checkDuplicate && checkDuplicate?.sub_category_id !== sub_category_id) {
      result = {
        message: 'sub_category_name already exist for this category',
        status: false,
        data: null,
      };
      return result;
    }
    const subCategoryDetails = await subCategoryDao.edit(
      name,
      category_id,
      budget,
      updated_by,
      sub_category_id
    );
    result = { success: true, data: subCategoryDetails };
    return result;
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
      result = { message: 'success', status: true, data: subCategoryData };
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
    const subCategoryData = { message: 'success', status: true, data: result };
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
    const subSubCategoryExistForThisSubCategory =
      await subSubCategoryDao.getBySubCategoryId(subCategoryId);

    if (!subCategoryExist) {
      const result = {
        status: false,
        message: 'sub_category_id does Not Exist',
        data: null,
      };
      return result;
    }

    if (subSubCategoryExistForThisSubCategory) {
      const result = {
        status: false,
        message:
          'unable to delete this sub_category.please delete the associated sub_sub_category to delete this record',
        data: null,
      };
      return result;
    }

    const data = await subCategoryDao.deleteSubCategory(subCategoryId);
    if (data) {
      const result = {
        status: true,
        message: 'SubCategory Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this subCategory',
        data: null,
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

/**
 * Method to Get All In Active SubCategory's
 * @returns
 */
const getAllInActiveSubCategories = async () => {
  try {
    const result = await subCategoryDao.getAllInActiveSubCategories();
    const subCategoryData = {
      message: 'success',
      status: true,
      data: result,
    };
    return subCategoryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllInActiveSubCategories subCategory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Sub Category - Pagination API
 * @returns
 */
const searchSubCategory = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const name = body.search_by_name;

    const status = body.status;
    const filterObj = {
      filterSubCategory: {
        AND: [],
        OR: [
          {
            name: {
              contains: name,
              mode: 'insensitive',
            },
          },
          {
            category: {
              name: {
                contains: name,
                mode: 'insensitive',
              },
            },
          },
        ],
        is_delete: status === 'IN' ? true : false,
      },
    };

    const result = await subCategoryDao.searchSubCategory(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempSubCategoryData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      limit: limit,
      content: data,
    };
    return tempSubCategoryData;
  } catch (error) {
    console.log(
      'Error occurred in searchSubCategory sub category service : ',
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
  getAllInActiveSubCategories,
  searchSubCategory,
};
