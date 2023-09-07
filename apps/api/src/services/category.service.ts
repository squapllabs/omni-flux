import bomConfigurationDao from '../dao/bomConfiguration.dao';
import categoryDao from '../dao/category.dao';
import projectDao from '../dao/project.dao';
import subCategoryDao from '../dao/subCategory.dao';
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
    const {
      name,
      project_id,
      budget,
      created_by = null,
      description,
      start_date,
      end_date,
      bom_configuration_id,
    } = body;
    let result = null;
    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }

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

    if (bom_configuration_id) {
      const bomConfigurationExist = await bomConfigurationDao.getById(
        bom_configuration_id
      );
      if (!bomConfigurationExist) {
        return {
          message: 'bom_configuration_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const categoryDetails = await categoryDao.add(
      name,
      project_id,
      budget,
      created_by,
      description,
      start_date,
      end_date,
      bom_configuration_id
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
    const {
      name,
      project_id,
      budget,
      updated_by,
      category_id,
      description,
      start_date,
      end_date,
      bom_configuration_id,
    } = body;
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
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }
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

    if (bom_configuration_id) {
      const bomConfigurationExist = await bomConfigurationDao.getById(
        bom_configuration_id
      );
      if (!bomConfigurationExist) {
        return {
          message: 'bom_configuration_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const categoryDetails = await categoryDao.edit(
      name,
      project_id,
      budget,
      updated_by,
      category_id,
      description,
      start_date,
      end_date,
      bom_configuration_id
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
        message: 'category_id does not exist',
        status: false,
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
    const subCategoryExistForThisCategory =
      await subCategoryDao.getByCategoryId(categoryId);
    if (!categoryExist) {
      const result = {
        message: 'category_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (subCategoryExistForThisCategory.length > 0) {
      const result = {
        message:
          'Unable to delete this category.Please delete the associated child category.',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await categoryDao.deleteCategory(categoryId);
    if (data) {
      const result = {
        message: 'Category Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this category',
        status: false,
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

/**
 * Method to search Category - Pagination API
 * @returns
 */
const searchCategory = async (body) => {
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
      filterCategory: {
        AND: [],
        name: {
          contains: name,
          mode: 'insensitive',
        },
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await categoryDao.searchCategory(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempCategoryData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      limit: limit,
      content: data,
    };
    return tempCategoryData;
  } catch (error) {
    console.log('Error occurred in searchCategory category service : ', error);
    throw error;
  }
};

/**
 * Method to get Category By project_id
 * @param project_id
 * @returns
 */
const getByProjectId = async (project_id: number) => {
  try {
    let result = null;
    const projectExist = await projectDao.getById(project_id);
    if (!projectExist) {
      result = {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const categoryData = await categoryDao.getByProjectId(project_id);
    if (categoryData.length > 0) {
      result = { message: 'success', status: true, data: categoryData };
      return result;
    } else {
      result = {
        message: 'No category found related to this category',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByProjectId category service : ', error);
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
  searchCategory,
  getByProjectId,
};
