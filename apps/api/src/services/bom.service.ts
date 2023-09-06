import bomDao from '../dao/bom.dao';
import categoryDao from '../dao/category.dao';
import subCategoryDao from '../dao/subCategory.dao';
import { bomBody } from '../interfaces/bom.interface';
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
      bom_type,
      machinery_id,
      labour_id
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
      machinery_id,
      labour_id,
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
      bom_type,
      machinery_id,
      labour_id
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

    /*       result = await prisma
      .$transaction(async (prisma) => {
        const userDetails = await userDao.add(
          md5(user_password),
          contact_no,
          email_id,
          first_name,
          last_name,
          user_status,
          created_by,
          department,
          parent_user_id,
          prisma
        );
        userDataWithRole.push({ userData: userDetails });

        if (userDetails) {
          const userRoleData = await userRoleDao.add(
            role_id,
            userDetails?.user_id,
            created_by,
            prisma
          );
          userDataWithRole.push({ userRoleData: userRoleData });
        }
        if (userDetails) {
          const userProfileData = await userProfileDao.add(
            userDetails?.user_id,
            profile_image_url,
            date_of_birth,
            gender,
            address,
            additional_info,
            created_by,
            prisma
          );
          userDataWithRole.push({ userProfileData: userProfileData });
        }
        return userDataWithRole;
      })
      .then((data) => {
        console.log('Successfully User Data Returned ', data);
        const newUserData = {
          message: 'success',
          status: true,
          data: data,
        };
        return newUserData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result; */

    const result = await prisma
      .$transaction(async (prisma) => {
        const bom = await bomDao.addBulk(body, prisma);
        const subCategoryBudget = await bomDao.getBomSumBySubCategoryId(
          sub_category_id,
          prisma
        );
        console.log('subCategoryBudget', typeof subCategoryBudget);

        const updated_by = body[0].updated_by
          ? body[0].updated_by
          : body[0].created_by
          ? body[0].created_by
          : null;
        /*    for (const bom of body) {
          if (!bom.is_delete) {
            subCategoryBudget += bom.total;
          }
        } */

        const subCategoryDetails = await subCategoryDao.updateBudget(
          subCategoryBudget,
          sub_category_id,
          updated_by,
          prisma
        );

        const category_id = subCategoryDetails?.category_id;
        /* let categoryBudget = 0; */

        /* const subCategoryDataByCategoryId = await subCategoryDao.getByCategoryId(
          category_id
        ); */

        const subCategoryDataByCategoryId =
          await subCategoryDao.getSumOfBudgetByCategoryId(category_id, prisma);

        /* for (const subCategory of subCategoryDataByCategoryId) {
          if (!subCategory.is_delete) {
            categoryBudget += subCategory.budget;
          }
        } */

        const categoryDetails = await categoryDao.updateBudget(
          subCategoryDataByCategoryId,
          category_id,
          updated_by,
          prisma
        );

        const data = {
          bom: bom,
          sub_category_details: subCategoryDetails,
          category_details: categoryDetails,
        };

        return data;

        // if (bom) {
        //   return {
        //     message: 'success',
        //     status: true,
        //     data: data,
        //   };
        // }
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
    // const bom = await bomDao.addBulk(body);
    // const subCategoryBudget = await bomDao.getBomSumBySubCategoryId(
    //   sub_category_id
    // );
    // const updated_by = body[0].updated_by
    //   ? body[0].updated_by
    //   : body[0].created_by
    //   ? body[0].created_by
    //   : null;
    // /*    for (const bom of body) {
    //   if (!bom.is_delete) {
    //     subCategoryBudget += bom.total;
    //   }
    // } */

    // const subCategoryDetails = await subCategoryDao.updateBudget(
    //   subCategoryBudget.data,
    //   sub_category_id,
    //   updated_by
    // );

    // const category_id = subCategoryDetails?.category_id;
    // /* let categoryBudget = 0; */

    // /* const subCategoryDataByCategoryId = await subCategoryDao.getByCategoryId(
    //   category_id
    // ); */

    // const subCategoryDataByCategoryId =
    //   await subCategoryDao.getSumOfBudgetByCategoryId(category_id);

    // /* for (const subCategory of subCategoryDataByCategoryId) {
    //   if (!subCategory.is_delete) {
    //     categoryBudget += subCategory.budget;
    //   }
    // } */

    // const categoryDetails = await categoryDao.updateBudget(
    //   subCategoryDataByCategoryId.data,
    //   category_id,
    //   updated_by
    // );

    // const data = {
    //   bom: bom,
    //   sub_category_details: subCategoryDetails,
    //   category_details: categoryDetails,
    // };

    // if (bom) {
    //   return {
    //     message: 'success',
    //     status: true,
    //     data: data,
    //   };
    // }
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

    const bomData = await bomDao.getBomTotalBySubCategoryId(sub_category_id);
    // const bomData = await bomDao.getBomSumBySubCategoryId(sub_category_id);

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
  getBomTotalBySubCategoryId,
};
