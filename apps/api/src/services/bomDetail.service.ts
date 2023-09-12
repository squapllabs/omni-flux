import bomConfigurationDao from '../dao/bomConfiguration.dao';
import bomDetailDao from '../dao/bomDetail.dao';
import categoryDao from '../dao/category.dao';
import projectDao from '../dao/project.dao';
import subCategoryDao from '../dao/subCategory.dao';
import { bomBody } from '../interfaces/bomDetail.interface';
import prisma from '../utils/prisma';

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
      machinery_id,
      labour_id,
      bom_configuration_id,
    } = body;

    const bomData = await bomDetailDao.add(
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
      machinery_id,
      labour_id,
      bom_configuration_id
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
      bom_detail_id,
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
      machinery_id,
      labour_id,
      bom_configuration_id,
    } = body;
    let result = null;
    const bomExist = await bomDetailDao.getById(bom_detail_id);
    if (!bomExist) {
      result = {
        message: 'bom_detail_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const bomDetails = await bomDetailDao.edit(
      bom_detail_id,
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
      machinery_id,
      labour_id,
      bom_configuration_id
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
    const result = await bomDetailDao.getAll();
    const bomData = { message: 'success', status: true, data: result };
    return bomData;
  } catch (error) {
    console.log('Error occurred in getAllBom Bom.service : ', error);
    throw error;
  }
};

/**
 * Method to get Bom By BomId
 * @param bomDetailId
 * @returns
 */
const fetchBomById = async (bom_detail_id: number) => {
  try {
    let result = null;
    const bomData = await bomDetailDao.getById(bom_detail_id);
    if (bomData) {
      result = {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      result = {
        message: 'bom_detail_id does not exist',
        status: false,
        data: null,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occurred in bom service fetchBomById ', error);
    throw error;
  }
};

/**
 * Method to delete bom
 * @param bomDetailId
 */
const deleteBomById = async (bom_detail_id: number) => {
  try {
    let result = null;
    const bomExist = await bomDetailDao.getById(bom_detail_id);
    if (!bomExist) {
      result = {
        message: 'bom_detail_id does not exist',
        status: false,
        data: null,
      };
    } else {
      const data = await bomDetailDao.deleteBom(bom_detail_id);
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
    const bomData = await bomDetailDao.getByCategorySubCatAndSubSubCatId(
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
 * @param bomDetailId
 */
const getEntireDataByBomId = async (bom_detail_id: number) => {
  try {
    let result = null;
    const bomExist = await bomDetailDao.getById(bom_detail_id);
    if (!bomExist) {
      result = {
        message: 'bom_detail_id does not exist',
        status: false,
        data: null,
      };
    } else {
      const bomEntireData = await bomDetailDao.entireData(bom_detail_id);
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
    const bom_configuration_id = body[0].bom_configuration_id;

    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      return {
        message: 'sub_category_id does not exist',
        status: false,
        data: null,
      };
    }
    const result = await prisma
      .$transaction(async (prisma) => {
        const bom = await bomDetailDao.addBulk(body, prisma);
        const subCategoryBudget = await bomDetailDao.getBomSumBySubCategoryId(
          sub_category_id,
          prisma
        );

        const updated_by = body[0].updated_by
          ? body[0].updated_by
          : body[0].created_by
          ? body[0].created_by
          : null;

        const subCategoryDetails = await subCategoryDao.updateBudget(
          subCategoryBudget,
          sub_category_id,
          updated_by,
          prisma
        );

        const category_id = subCategoryDetails?.category_id;

        const subCategoryDataByCategoryId =
          await subCategoryDao.getSumOfBudgetByCategoryId(category_id, prisma);

        const categoryDetails = await categoryDao.updateBudget(
          subCategoryDataByCategoryId,
          category_id,
          updated_by,
          prisma
        );

        const bomConfigurationBudget =
          await bomDetailDao.getTotalByBomConfigurationId(
            bom_configuration_id,
            prisma
          );

        const bomConfigurationDetails = await bomConfigurationDao.updateBudget(
          bomConfigurationBudget,
          bom_configuration_id,
          updated_by,
          prisma
        );

        const data = {
          bom: bom,
          sub_category_details: subCategoryDetails,
          category_details: categoryDetails,
          bom_configuration_details: bomConfigurationDetails,
        };

        return data;
      })
      .then((data) => {
        console.log('Successfully BOM Data Returned ', data);
        const bomData = {
          message: 'success',
          status: true,
          data: data,
        };
        return bomData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in Bom service addBulkBom : ', error);
    throw error;
  }
};

/**
 * Method to get Bom By SubCategoryId And BomType
 * @param sub_category_id
 * @param bom_type
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

    const bomData = await bomDetailDao.getBomBySubCategoryIdAndBomType(
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

/**
 * Method to get Bom Total Value By SubCategoryId
 * @param sub_category_id
 * @returns
 */
const getBomTotalBySubCategoryId = async (sub_category_id: number) => {
  try {
    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      return {
        message: 'sub_category_id does not exist ',
        status: false,
        data: null,
      };
    }

    const bomData = await bomDetailDao.getBomTotalBySubCategoryId(
      sub_category_id
    );

    if (bomData) {
      return {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      return {
        message: 'No data found for this sub_category_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in bom service getBomTotalBySubCategoryId : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get Bom By Sub Category Id
 * @param sub_category_id
 * @returns
 */
const getBySubCategoryId = async (sub_category_id: number) => {
  try {
    let result = null;

    const subCategoryExist = await subCategoryDao.getById(sub_category_id);
    if (!subCategoryExist) {
      return {
        message: 'sub_category_id does not exist',
        status: false,
        data: null,
      };
    }

    const bomData = await bomDetailDao.getBySubCategoryId(sub_category_id);
    if (bomData.length > 0) {
      result = {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      result = {
        message: 'There is no bom data related to this sub_category_id',
        status: false,
        data: null,
      };
    }
    return result;
  } catch (error) {
    console.log('Error occurred in bom service getBySubCategoryId ', error);
    throw error;
  }
};

/**
 * Method to get Bom By ProjectId And BomType
 * @param project_id
 * @param bom_type
 * @returns
 */
const getByProjectIdAndBomType = async (
  project_id: number,
  bom_type: string
) => {
  try {
    let result = null;

    const projectExist = await projectDao.getById(project_id);
    if (!projectExist) {
      return {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
    }

    const bomData = await bomDetailDao.getByProjectIdAndBomType(
      project_id,
      bom_type
    );
    if (bomData.length > 0) {
      result = {
        message: 'success',
        status: true,
        data: bomData,
      };
    } else {
      result = {
        message: 'There is no bom data related to this project_id and bom_type',
        status: false,
        data: null,
      };
    }
    return result;
  } catch (error) {
    console.log(
      'Error occurred in bom service getByProjectIdAndBomType ',
      error
    );
    throw error;
  }
};

export {
  createBom,
  updateBom,
  fetchBomById,
  getAllBom,
  deleteBomById,
  getByCategorySubCatAndSubSubCatId,
  getEntireDataByBomId,
  addBulkBom,
  getBomBySubCategoryIdAndBomType,
  getBomTotalBySubCategoryId,
  getBySubCategoryId,
  getByProjectIdAndBomType,
};
