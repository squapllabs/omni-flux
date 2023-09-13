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
    const {
      name,
      sub_category_id,
      budget,
      created_by = null,
      description,
      project_id,
      parent_sub_sub_category_id,
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
      created_by,
      description,
      project_id,
      parent_sub_sub_category_id
    );
    result = { message: 'success', status: true, data: subSubCategoryDetails };
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
    const {
      name,
      sub_category_id,
      budget,
      updated_by,
      sub_sub_category_id,
      description,
      project_id,
      parent_sub_sub_category_id,
    } = body;
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
      sub_sub_category_id,
      description,
      project_id,
      parent_sub_sub_category_id
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
        message: 'sub_sub_category_id does not exist',
        staus: false,
        data: null
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
        message: 'sub_sub_category_id does Not Exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await subSubCategoryDao.deleteSubSubCategory(subSubCategoryId);
    if (data) {
      const result = {
        message: 'SubSubCategory Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this subSubCategory',
        status: false,
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

/**
 * Method to search Sub Sub Category - Pagination API
 * @returns
 */
const searchSubSubCategory = async (body) => {
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
      filterSubSubCategory: {
        AND: [],
        OR: [
          {
            name: {
              contains: name,
              mode: 'insensitive',
            },
          },
          {
            sub_category: {
              name: {
                contains: name,
                mode: 'insensitive',
              },
            },
          },
          {
            sub_category: {
              category: {
                name: {
                  contains: name,
                  mode: 'insensitive',
                },
              },
            },
          },
          {
            parent_data: {
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

    const result = await subSubCategoryDao.searchSubSubCategory(
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
      'Error occurred in searchSubSubCategory sub sub category service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get SubSubCategory By subCategoryId
 * @param subCategoryId
 * @returns
 */
const getBySubCategoryId = async (subCategoryId: number) => {
  try {
    let result = null;
    const subCategoryData = await subCategoryDao.getById(subCategoryId);
    if (!subCategoryData) {
      result = {
        message: 'sub_category_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const subSubCategoryData = await subSubCategoryDao.getBySubCategoryId(
      subCategoryId
    );
    if (subSubCategoryData.length > 0) {
      result = {
        message: 'success',
        status: true,
        data: subSubCategoryData,
      };
      return result;
    } else {
      result = {
        message: 'sub_sub_category data not exist for this sub_category_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getBySubCategoryId subSubCategory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get All Parent SubSubCategory
 * @returns
 */
const getAllParentData = async () => {
  try {
    let result = null;
    const subSubCategoryData = await subSubCategoryDao.getAllParentData();
    result = {
      message: 'success',
      status: true,
      data: subSubCategoryData,
    };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in getAllParentData subSubCategory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get Child Sub Sub Category By parent_sub_sub_category_id
 * @returns
 */
const getChildDataByParentSubSubCatId = async (
  parent_sub_sub_category_id: number
) => {
  try {
    let result = null;

    const subSubCategoryData = await subSubCategoryDao.getById(
      parent_sub_sub_category_id
    );
    if (!subSubCategoryData) {
      result = {
        message: 'parent_sub_sub_category_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const childSubSubCategoryData =
      await subSubCategoryDao.getChildDataByParentSubSubCatId(
        parent_sub_sub_category_id
      );
    if (childSubSubCategoryData.length > 0) {
      result = {
        message: 'success',
        status: true,
        data: childSubSubCategoryData,
      };
      return result;
    } else {
      result = {
        message: 'There is no child data related to this parent_sub_sub_category_id',
        status: false,
        data: childSubSubCategoryData,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getChildDataByParentSubSubCatId subSubCategory service : ',
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
  searchSubSubCategory,
  getBySubCategoryId,
  getAllParentData,
  getChildDataByParentSubSubCatId,
};
