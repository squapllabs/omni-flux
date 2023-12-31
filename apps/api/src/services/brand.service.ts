import brandDao from '../dao/brand.dao';
import { createBrandBody, updateBrandBody } from '../interfaces/brand.interface';
import prisma from '../utils/prisma';

/**
 * Method to Add a new brand
 * @param body
 * @returns
 */
const addBrand = async (body: createBrandBody) => {
  let result = null;
  try {
    const brand_id = body.brand_id;
    const brand_name = body.brand_name;
    const created_by = body.created_by;
    const updated_by = body.updated_by;
    result = await prisma
      .$transaction(async (prisma) => {
        const CreateBrand = await brandDao.add(
          brand_id,
          brand_name,
          created_by,
          updated_by,
          prisma
        );

        return CreateBrand;
      })
      .then((data) => {
        console.log('Successfully brand Data Returned ', data);
        const newBrandData = {
          message: 'success', status: true,
          data: data,
        };
        return newBrandData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in brand service: ', error);
    throw error;
  }
};
/**
 * Method to Get All brand
 * @returns
 */
const getAllBrand = async () => {
  try {
    const result = await brandDao.getAll(
    );
    const brandData = { message: 'success', status: true, total_count: result.totalCount, data: result.brands };
    return brandData;
  } catch (error) {
    console.log('Error occurred in getAll brand service: ', error);
    throw error;
  }
};

/**
 * Method to get brand By brandId
 * @param brandId
 * @returns
 */
const getById = async (brand_id: number) => {
  try {
    let result = null;
    const brandData = await brandDao.getById(brand_id);
    if (brandData) {
      result = { message: 'success', status: true, data: brandData };
      return result;
    } else {
      result = { message: 'brand Id not exist', status: true, data: null };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById brand service : ', error);
    throw error;
  }
};
/**
* Method to delete brand
* @param brandId
*/
const deleteBrand = async (brand_id: number) => {
  try {
    const brandExist = await brandDao.getById(brand_id);
    if (!brandExist) {
      const result = { message: 'brand Id Not Exist', status: true, data: null };
      return result;
    }
    const data = await brandDao.deleteBrand(brand_id);
    if (data) {
      const result = {
        message: 'successfully delete this brand', status: true,
        data: null,
      };
      return result;
    } else {
      const result = { message: 'Failed to delete this brand', status: true, data: null };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteBrand brand service : ', error);
    throw error;
  }
};
/**
* Method to Update an Existing brand
* @param body
* @returns
*/
const updateBrand = async (body: updateBrandBody) => {
  try {
    const brand_id = body.brand_id;
    const brand_name = body.brand_name;
    const updated_by = body.updated_by;
    let result = null;
    const brandExist = await brandDao.getById(brand_id);
    if (brandExist) {
      const brandDetails = await brandDao.edit(
        brand_id,
        brand_name,
        updated_by,
      );
      result = { message: 'success', status: true, data: brandDetails };
      return result;
    } else {
      result = { message: 'brand_id not exist', status: true, data: null };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in brand service Edit: ', error);
    throw error;
  }
};
export {
  addBrand,
  getAllBrand,
  getById,
  deleteBrand,
  updateBrand
}