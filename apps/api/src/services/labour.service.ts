import labourDao from '../dao/labour.dao';
import { labourBody } from '../interfaces/labour.inteface';

/**
 * Method to Create a New Labour
 * @param body
 * @returns
 */
const createLabour = async (body: labourBody) => {
  try {
    const { labour_type, rate, uom_id, created_by } = body;
    const labourDetails = await labourDao.add(
      labour_type,
      rate,
      uom_id,
      created_by
    );
    const result = { message: 'success', status: true, data: labourDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in labour service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Labour
 * @param body
 * @returns
 */

const updateLabour = async (body: labourBody) => {
  try {
    const { labour_type, rate, uom_id, updated_by, labour_id } = body;
    let result = null;
    const labourExist = await labourDao.getById(labour_id);
    if (!labourExist) {
      result = {
        message: 'labour_id does not exist',
        status: false,
        data: null,
      };
      return result;
    } else {
      const labourDetails = await labourDao.edit(
        labour_id,
        labour_type,
        rate,
        uom_id,
        updated_by
      );
      result = { message: 'success', status: true, data: labourDetails };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in labour service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Labour By LabourId
 * @param labourId
 * @returns
 */
const getById = async (labourId: number) => {
  try {
    let result = null;
    const labourData = await labourDao.getById(labourId);
    if (labourData) {
      result = { message: 'success', status: true, data: labourData };
      return result;
    } else {
      result = {
        message: 'labour_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById labour service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Labours
 * @returns
 */
const getAllLabours = async () => {
  try {
    const result = await labourDao.getAll();
    const labourData = { message: 'success', status: true, data: result };
    return labourData;
  } catch (error) {
    console.log('Error occurred in getAllLabours labour service : ', error);
    throw error;
  }
};

/**
 * Method to delete labour
 * @param labourId
 */
const deleteLabour = async (labourId: number) => {
  try {
    const labourExist = await labourDao.getById(labourId);

    if (!labourExist) {
      const result = {
        message: 'labour_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await labourDao.deleteLabour(labourId);
    if (data) {
      const result = {
        message: 'Labour Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this labour',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteLabour labour service : ', error);
    throw error;
  }
};

/**
 * Method to search Labour - Pagination API
 * @returns
 */
const searchLabour = async (body) => {
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
    const filterObj: any = {};

    if (status) {
      filterObj.filterLabour = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (global_search) {
      filterObj.filterLabour = filterObj.filterLabour || {};
      filterObj.filterLabour.OR = filterObj.filterLabour.OR || [];

      filterObj.filterLabour.OR.push(
        {
          labour_type: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          uom: {
            name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await labourDao.searchLabour(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempLabourData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempLabourData;
  } catch (error) {
    console.log('Error occurred in searchLabour Labour service : ', error);
    throw error;
  }
};

export {
  createLabour,
  updateLabour,
  getAllLabours,
  getById,
  deleteLabour,
  searchLabour,
};
