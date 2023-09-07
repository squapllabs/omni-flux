import subCategoryDao from '../dao/subCategory.dao';
import categoryDao from '../dao/category.dao';
import {
  createSubCategoryBody,
  updateSubCategoryBody,
} from '../interfaces/subCategory.Interface';
import subSubCategoryDao from '../dao/subSubCategory.dao';
import projectDao from '../dao/project.dao';
import bomConfigurationDao from '../dao/bomConfiguration.dao';

/**
 * Method to Create a New SubCategory
 * @param body
 * @returns
 */
const createSubCategory = async (body: createSubCategoryBody) => {
  try {
    const {
      name,
      category_id,
      budget,
      created_by = null,
      description,
      project_id,
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

    const subCategoryDetails = await subCategoryDao.add(
      name,
      category_id,
      budget,
      created_by,
      description,
      project_id,
      start_date,
      end_date,
      bom_configuration_id
    );
    result = { message: 'success', status: true, data: subCategoryDetails };
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
    const {
      name,
      category_id,
      budget,
      updated_by,
      sub_category_id,
      description,
      project_id,
      start_date,
      end_date,
      bom_configuration_id,
    } = body;
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

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
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

    const categoryExist = await categoryDao.getById(category_id);
    if (!categoryExist) {
      result = {
        message: 'category_id does not exist',
        status: false,
        data: null,
      };
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
      sub_category_id,
      description,
      project_id,
      start_date,
      end_date,
      bom_configuration_id
    );
    result = { message: 'success', status: true, data: subCategoryDetails };
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
      result = {
        status: false,
        data: null,
        message: 'sub_category_id does not exist',
      };
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

    if (subSubCategoryExistForThisSubCategory.length > 0) {
      const result = {
        status: false,
        message:
          'Unable to delete this sub category.Please delete the associated child category.',
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
          {
            project_data: {
              project_name: {
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

/**
 * Method to get SubCategory By categoryId
 * @param categoryId
 * @returns
 */
const getByCategoryId = async (categoryId: number) => {
  try {
    let result = null;
    const categoryData = await categoryDao.getById(categoryId);
    if (!categoryData) {
      result = {
        message: 'category_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const subCategoryData = await subCategoryDao.getByCategoryId(categoryId);
    if (subCategoryData.length > 0) {
      result = {
        message: 'success',
        status: true,
        data: subCategoryData,
      };
      return result;
    } else {
      result = {
        message: 'sub_category data not exist for this category_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByCategoryId subCategory service : ',
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
  getByCategoryId,
};
