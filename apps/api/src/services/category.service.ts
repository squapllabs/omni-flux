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
    let result = null;
    if (project_id) {
      const categoryData = await categoryDao.getByCategoryNameAndProjectId(
        name,
        Number(project_id)
      );
      if (categoryData) {
        result = {
          message: 'category_name already exist for this project',
          status: false,
          data: null,
        };
        return result;
      }
    }
    const categoryDetails = await categoryDao.add(
      name,
      project_id,
      budget,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: categoryDetails,
    };
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
    if (!categoryExist) {
      result = {
        message: 'category_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (project_id) {
      const categoryData = await categoryDao.getByCategoryNameAndProjectId(
        name,
        Number(project_id)
      );
      if (categoryData && categoryData?.category_id !== category_id) {
        result = {
          message: 'category_name already exist for this project',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const categoryDetails = await categoryDao.edit(
      name,
      project_id,
      budget,
      updated_by,
      category_id
    );
    result = {
      message: 'success',
      status: true,
      data: categoryDetails,
    };
    return result;
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
      result = { message: 'success', status: true, data: categoryData };
      return result;
    } else {
      result = {
        status: false,
        message: 'category_id does not exist',
        data: null,
      };
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
    const categoryData = { message: 'success', status: true, data: result };
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
      const result = {
        status: false,
        message: 'category_id does not exist',
        data: null,
      };
      return result;
    }
    const data = await categoryDao.deleteCategory(categoryId);
    if (data) {
      const result = {
        status: true,
        message: 'Category Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this category',
        data: null,
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

/**
 * Method to Get All InActive Category's
 * @returns
 */
const getAllInActiveCategories = async () => {
  try {
    const result = await categoryDao.getAllInActiveCategories();
    const categoryData = { message: 'success', status: true, data: result };
    return categoryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllInActiveCategories category service : ',
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
  getAllInActiveCategories,
};
