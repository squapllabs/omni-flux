import gstDao from '../dao/gst.dao';
import { createGstBody, updateGstBody } from '../interfaces/gst.Interface';
import itemDao from '../dao/item.dao';

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
    const result = { message: 'success', status: true, data: gstDetails };
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
      result = { message: 'success', status: true, data: gstDetails };
      return result;
    } else {
      result = { message: 'gst_id not exist', status: false, data: null };
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
      result = { message: 'success', status: true, data: gstData };
      return result;
    } else {
      result = { message: 'gst id not exist', status: false, data: null };
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
    const gstData = { message: 'success', status: true, data: result };
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
      const result = {
        message: 'gst_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const gstExistInItem = await itemDao.getByGSTId(gstId);
    if (gstExistInItem) {
      const result = {
        message: 'unable to delete.This gst_id is mapped in item table',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await gstDao.deleteGst(gstId);
    if (data) {
      const result = {
        message: 'Gst Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this gst',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteGst gst service : ', error);
    throw error;
  }
};

/**
 * Method to search Gst - Pagination API
 * @returns
 */
const searchGst = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;
    const filterObj = {
      filterGst: {
        AND: [],
        OR: [
          { rate: global_search },
          { cgst_rate: global_search },
          { igst_rate: global_search },
          { sgst_rate: global_search },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await gstDao.searchGST(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempGstData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempGstData;
  } catch (error) {
    console.log('Error occurred in searchGst Gst service : ', error);
    throw error;
  }
};

export { createGst, updateGst, getAllGst, getById, deleteGst, searchGst };
