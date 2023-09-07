import machineryDao from '../dao/machinery.dao';
import uomDao from '../dao/uom.dao';
import { machineryBody } from '../interfaces/machinery.interface';

/**
 * Method to Create a New Machinery
 * @param body
 * @returns
 */
const createMachinery = async (body: machineryBody) => {
  try {
    const {
      machinery_name,
      machinery_model,
      machinery_type,
      manufacturer,
      date_of_purchase,
      warranty_expired_on,
      operational_status,
      location,
      rate,
      uom_id,
      created_by,
    } = body;
    let result = null;

    if (uom_id) {
      const uomExist = await uomDao.getById(uom_id);
      if (!uomExist) {
        return {
          message: 'uom_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const machineryDetails = await machineryDao.add(
      machinery_name,
      machinery_model,
      machinery_type,
      manufacturer,
      date_of_purchase,
      warranty_expired_on,
      operational_status,
      location,
      rate,
      uom_id,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: machineryDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in machinery service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Machinery
 * @param body
 * @returns
 */
const updateMachinery = async (body: machineryBody) => {
  try {
    const {
      machinery_id,
      machinery_name,
      machinery_model,
      machinery_type,
      manufacturer,
      date_of_purchase,
      warranty_expired_on,
      operational_status,
      location,
      rate,
      uom_id,
      updated_by,
    } = body;
    let result = null;

    const machineryExist = await machineryDao.getById(machinery_id);
    if (!machineryExist) {
      result = {
        message: 'machinery_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (uom_id) {
      const uomExist = await uomDao.getById(uom_id);
      if (!uomExist) {
        return {
          message: 'uom_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const machineryDetails = await machineryDao.edit(
      machinery_id,
      machinery_name,
      machinery_model,
      machinery_type,
      manufacturer,
      date_of_purchase,
      warranty_expired_on,
      operational_status,
      location,
      rate,
      uom_id,
      updated_by
    );
    result = {
      message: 'success',
      status: true,
      data: machineryDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in machinery service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Machinery By MachineryId
 * @param MachineryId
 * @returns
 */
const getById = async (machineryId: number) => {
  try {
    let result = null;
    const machineryData = await machineryDao.getById(machineryId);
    if (machineryData) {
      result = { message: 'success', status: true, data: machineryData };
      return result;
    } else {
      result = {
        message: 'machinery_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById machinery service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Machinery's
 * @returns
 */
const getAllMachinery = async () => {
  try {
    const result = await machineryDao.getAll();
    const machineryData = { message: 'success', status: true, data: result };
    return machineryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllmachineryData machinery service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete Machinery
 * @param MachineryId
 */
const deleteMachinery = async (machineryId: number) => {
  try {
    const machineryExist = await machineryDao.getById(machineryId);

    if (!machineryExist) {
      const result = {
        message: 'machinary_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await machineryDao.deleteById(machineryId);
    if (data) {
      const result = {
        message: 'Machinery Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this machinery',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteMachinery machinery service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Machinery - Pagination API
 * @returns
 */
const searchMachinery = async (body) => {
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
      filterMachinery: {
        AND: [],
        OR: [
          {
            machinery_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            machinery_model: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            machinery_type: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            manufacturer: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            operational_status: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            location: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            uom_data: {
              name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await machineryDao.searchMachinery(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempMachineryData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempMachineryData;
  } catch (error) {
    console.log(
      'Error occurred in searchMachinery Machinery service : ',
      error
    );
    throw error;
  }
};

export {
  createMachinery,
  updateMachinery,
  getAllMachinery,
  getById,
  deleteMachinery,
  searchMachinery,
};
