import categoryDao from '../dao/category.dao';
import {
  createCategoryBody,
  updateCategoryBody,
} from '../interfaces/category.Interface';

/**
 * Method to Create a New Category
 * @param body
 * @returns
 */
const createCategory = async (body: createCategoryBody) => {
  try {
    const { name, project_id, budget, created_by = null } = body;
    const categoryDetails = await categoryDao.add(
      name,
      project_id,
      budget,
      created_by
    );
    const result = { success: true, data: categoryDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in category service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Category
 * @param body
 * @returns
 */
const updateCategory = async (body: updateCategoryBody) => {
  try {
    const { name, project_id, budget, updated_by, category_id } = body;
    let result = null;
    const categoryExist = await categoryDao.getById(category_id);
    if (categoryExist) {
      const categoryDetails = await categoryDao.edit(
        name,
        project_id,
        budget,
        updated_by,
        category_id
      );
      result = { success: true, data: categoryDetails };
      return result;
    } else {
      result = { success: false, message: 'category_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in category service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Category By categoryId
 * @param categoryId
 * @returns
 */
const getById = async (categoryId: number) => {
  try {
    let result = null;
    const categoryData = await categoryDao.getById(categoryId);
    if (categoryData) {
      result = { success: true, data: categoryData };
      return result;
    } else {
      result = { success: false, message: 'category id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById category service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Category's
 * @returns
 */
const getAllCategory = async () => {
  try {
    const result = await categoryDao.getAll();
    const categoryData = { success: true, data: result };
    return categoryData;
  } catch (error) {
    console.log('Error occurred in getAllCategory category service : ', error);
    throw error;
  }
};

/**
 * Method to delete category
 * @param categoryId
 */
const deleteCategory = async (categoryId: number) => {
  try {
    const categoryExist = await categoryDao.getById(categoryId);
    if (!categoryExist) {
      const result = { success: false, message: 'Category Id Not Exist' };
      return result;
    }
    const data = await categoryDao.deleteCategory(categoryId);
    if (data) {
      const result = {
        success: true,
        message: 'Category Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this category',
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteCategory category service : ', error);
    throw error;
  }
};

/**
 * Method to check Duplicate Project Category Name by category_name and project_id
 * @param categoryName
 * @param projectId
 * @returns
 */
const checkDuplicateProjectCategoryName = async (
  categoryName: string,
  projectId: number
) => {
  try {
    let result = null;
    const categoryData = await categoryDao.getByCategoryNameAndProjectId(
      categoryName,
      Number(projectId)
    );
    if (categoryData) {
      result = {
        message: 'success',
        status: true,
        is_exist: true,
        data: categoryData,
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
      'Error occurred in checkDuplicateProjectCategoryName category service : ',
      error
    );
    throw error;
  }
};

export {
  createCategory,
  updateCategory,
  getAllCategory,
  getById,
  deleteCategory,
  checkDuplicateProjectCategoryName,
};
