import bomDao from '../dao/bom.dao';
import categoryDao from '../dao/category.dao';
import subCategoryDao from '../dao/subCategory.dao';
import { bomBody } from '../interfaces/bom.interface';

/**
 * Method to Create a New Bom
 * @param body
 * @returns
 */
const createBom = async (body: bomBody) => {
  try {
    let result = null;
    const {
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      created_by,
      description,
      rate,
      total,
      bom_type,
    } = body;

    const bomData = await bomDao.add(
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      created_by,
      description,
      rate,
      total,
      bom_type
    );
    if (bomData) {
      result = {
        message: 'success',
        status: true,
        data: bomData,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occured in bom Service createBom', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Bom
 * @param body
 * @returns
 */
const updateBom = async (body: bomBody) => {
  try {
    const {
      bom_id,
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      updated_by,
      description,
      rate,
      total,
      bom_type,
    } = body;
    let result = null;
    const bomExist = await bomDao.getById(bom_id);
    if (!bomExist) {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const bomDetails = await bomDao.edit(
      bom_id,
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      updated_by,
      description,
      rate,
      total,
      bom_type
    );
    result = {
      message: 'success',
      status: true,
      data: bomDetails,
    };

    return result;
  } catch (error) {
    console.log('Error occured in updateBom Bom service', error);
    throw error;
  }
};

/**
 * Method to Get All Bom's
 * @returns
 */
const getAllBom = async () => {
  try {
    const result = await bomDao.getAll();
    const bomData = { message: 'success', status: true, data: result };
    return bomData;
  } catch (error) {
    console.log('Error occurred in getAllBom Bom.service : ', error);
    throw error;
  }
};

/**
 * Method to get Bom By BomId
 * @param bomId
 * @returns
 */
const fetchBomById = async (bom_id: number) => {
  try {
    let result = null;
    const bomData = await bomDao.getById(bom_id);
    if (bomData) {
      result = {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occurred in bom_id', error);
    throw error;
  }
};

/**
 * Method to delete bom
 * @param bomId
 */
const deleteBomById = async (bom_id: number) => {
  try {
    let result = null;
    const bomExist = await bomDao.getById(bom_id);
    if (!bomExist) {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
    } else {
      const data = await bomDao.deleteBom(bom_id);
      if (data) {
        result = {
          message: 'Bom Data Successfully Deleted',
          status: true,
          data: null,
        };
      } else {
        result = {
          message: 'Failed to delete this bom',
          status: false,
          data: null,
        };
      }
    }
    return result;
  } catch (error) {
    console.log('Error occurred in delete bom.service', error);
    throw error;
  }
};

/**
 * Method to get BOM By Category Id Sub Category ID And Sub Sub Category Id
 * @param body
 * @returns
 */
const getByCategorySubCatAndSubSubCatId = async (body) => {
  try {
    let result = null;
    const {
      category_id,
      sub_category_id = null,
      sub_sub_category_id = null,
    } = body;
    const bomData = await bomDao.getByCategorySubCatAndSubSubCatId(
      category_id,
      sub_category_id,
      sub_sub_category_id
    );
    result = { message: 'success', status: true, data: bomData };
    return result;
  } catch (error) {
    console.log(
      'Error occurred in getByCategorySubCatAndSubSubCatId bom service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to entireData bom
 * @param bomId
 */
const getEntireDataByBomId = async (bom_id: number) => {
  try {
    let result = null;
    const bomExist = await bomDao.getById(bom_id);
    if (!bomExist) {
      result = {
        message: 'bom_id does not exist',
        status: false,
        data: null,
      };
    } else {
      const bomEntireData = await bomDao.entireData(bom_id);
      result = {
        message: 'success',
        status: true,
        data: bomEntireData,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occurred in getEntireDataByBomId bom.service', error);
    throw error;
  }
};

/**
 * Method to add bulk Bom
 * @param Array[]
 */
const addBulkBom = async (body) => {
  try {
    const sub_category_id = body[0].sub_category_id;

    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      return {
        message: 'sub_category_id does not exist',
        status: false,
        data: null,
      };
    }

    const bom = await bomDao.addBulk(body);
    let subCategoryBudget = 0;
    const updated_by = body[0].updated_by
      ? body[0].updated_by
      : body[0].created_by
      ? body[0].created_by
      : null;
    for (const bom of body) {
      if (!bom.is_delete) {
        subCategoryBudget += bom.total;
      }
    }

    const subCategoryDetails = await subCategoryDao.updateBudget(
      subCategoryBudget,
      sub_category_id,
      updated_by
    );

    const category_id = subCategoryDetails?.category_id;
    let categoryBudget = 0;

    const subCategoryDataByCategoryId = await subCategoryDao.getByCategoryId(
      category_id
    );

    for (const subCategory of subCategoryDataByCategoryId) {
      if (!subCategory.is_delete) {
        categoryBudget += subCategory.budget;
      }
    }

    const categoryDetails = await categoryDao.updateBudget(
      categoryBudget,
      category_id,
      updated_by
    );

    const data = {
      bom: bom,
      sub_category_details: subCategoryDetails,
      category_details: categoryDetails,
    };

    if (bom) {
      return {
        message: 'success',
        status: true,
        data: data,
      };
    }
  } catch (error) {
    console.log('Error occurred in Bom service addBulkBom : ', error);
    throw error;
  }
};

/**
 * Method to get Bom By SubCategoryId And BomType
 * @param bomId
 * @returns
 */
const getBomBySubCategoryIdAndBomType = async (
  sub_category_id: number,
  bom_type: string
) => {
  try {
    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      return {
        message: 'sub_category_id does not exist ',
        status: false,
        data: null,
      };
    }

    const bomData = await bomDao.getBomBySubCategoryIdAndBomType(
      sub_category_id,
      bom_type
    );
    if (bomData.length > 0) {
      return {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      return {
        message:
          'No data found for this sub_category_id and bom_type combination',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in bom service getBomBySubCategoryIdAndBomType : ',
      error
    );
    throw error;
  }
};

export {
  deleteBomById,
  createBom,
  updateBom,
  fetchBomById,
  getAllBom,
  getByCategorySubCatAndSubSubCatId,
  getEntireDataByBomId,
  addBulkBom,
  getBomBySubCategoryIdAndBomType,
};
