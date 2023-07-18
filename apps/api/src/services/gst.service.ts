import gstDao from '../dao/gst.dao';
import { createGstBody, updateGstBody } from '../interfaces/gst.Interface';

/**
 * Method to Create a New Gst
 * @param body
 * @returns
 */
const createGst = async (body: createGstBody) => {
  try {
    const { rate, cgst_rate, igst_rate, sgst_rate, created_by = null } = body;
    const gstDetails = await gstDao.add(
      rate,
      cgst_rate,
      igst_rate,
      sgst_rate,
      created_by
    );
    const result = { success: true, data: gstDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in gst service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Gst
 * @param body
 * @returns
 */
const updateGst = async (body: updateGstBody) => {
  try {
    const { rate, cgst_rate, igst_rate, sgst_rate, updated_by, gst_id } = body;
    let result = null;
    const gstExist = await gstDao.getById(gst_id);
    if (gstExist) {
      const gstDetails = await gstDao.edit(
        rate,
        cgst_rate,
        igst_rate,
        sgst_rate,
        updated_by,
        gst_id
      );
      result = { success: true, data: gstDetails };
      return result;
    } else {
      result = { success: false, message: 'gst_id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in gst service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Gst By GstId
 * @param gstId
 * @returns
 */
const getById = async (gstId: number) => {
  try {
    let result = null;
    const gstData = await gstDao.getById(gstId);
    if (gstData) {
      result = { success: true, data: gstData };
      return result;
    } else {
      result = { success: false, message: 'gst id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById gst service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Gst's
 * @returns
 */
const getAllGst = async () => {
  try {
    const result = await gstDao.getAll();
    const gstData = { success: true, data: result };
    return gstData;
  } catch (error) {
    console.log('Error occurred in getAllGst gst service : ', error);
    throw error;
  }
};

/**
 * Method to delete gst
 * @param gstId
 */
const deleteGst = async (gstId: number) => {
  try {
    const gstExist = await gstDao.getById(gstId);
    if (!gstExist) {
      const result = { success: false, message: 'Gst Id Not Exist' };
      return result;
    }
    const data = await gstDao.deleteGst(gstId);
    if (data) {
      const result = {
        success: true,
        message: 'Gst Data Deleted Successfully',
      };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this gst' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteGst gst service : ', error);
    throw error;
  }
};

export { createGst, updateGst, getAllGst, getById, deleteGst };
