import capabilityDao from '../dao/capability.dao';
import permissionsDao from '../dao/permissions.dao';
import { capabilityBody } from '../interfaces/capability.Interface';

/**
 * Method to Create a New Capability
 * @param body
 * @returns
 */
const createCapability = async (body: capabilityBody) => {
  try {
    const {
      capability_name,
      description,
      permission_type,
      ui_type,
      created_by,
    } = body;
    let result = null;

    const capabilityDetails = await capabilityDao.add(
      capability_name,
      description,
      permission_type,
      ui_type,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: capabilityDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in capability service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Capability
 * @param body
 * @returns
 */
const updateCapability = async (body: capabilityBody) => {
  try {
    const {
      capability_name,
      description,
      permission_type,
      ui_type,
      updated_by,
      capability_id,
    } = body;
    let result = null;

    const capabilityExist = await capabilityDao.getById(capability_id);
    if (!capabilityExist) {
      result = {
        message: 'capability_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const capabilityDetails = await capabilityDao.edit(
      capability_name,
      description,
      permission_type,
      ui_type,
      updated_by,
      capability_id
    );
    result = {
      message: 'success',
      status: true,
      data: capabilityDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in capability service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Capability By capabilityId
 * @param capabilityId
 * @returns
 */
const getById = async (capabilityId: number) => {
  try {
    let result = null;
    const capabilityData = await capabilityDao.getById(capabilityId);
    if (capabilityData) {
      result = { message: 'success', status: true, data: capabilityData };
      return result;
    } else {
      result = {
        status: false,
        message: 'capability_id does not exist',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById capability service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Capability's
 * @returns
 */
const getAllCapabilities = async () => {
  try {
    const result = await capabilityDao.getAll();
    const capabilityData = { message: 'success', status: true, data: result };
    return capabilityData;
  } catch (error) {
    console.log(
      'Error occurred in getAllCapabilities capability service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete capability
 * @param capabilityId
 */
const deleteCapability = async (capabilityId: number) => {
  try {
    const capabilityExist = await capabilityDao.getById(capabilityId);

    const capabilityExistInPermission = await permissionsDao.getByCapabilityId(
      capabilityId
    );

    if (!capabilityExist) {
      const result = {
        message: 'capability_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (capabilityExistInPermission) {
      const result = {
        message:
          'Unable to delete this capability.It is referenced in Permission Table',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await capabilityDao.deleteById(capabilityId);
    if (data) {
      const result = {
        status: true,
        message: 'Capability Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this capability',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteCapability capability service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Capability - Pagination API
 * @returns
 */
const searchCapability = async (body) => {
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
      filterCapability: {
        AND: [],
        OR: [
          {
            capability_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            description: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            permission_type: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            ui_type: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await capabilityDao.searchCapability(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempCapabilityData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempCapabilityData;
  } catch (error) {
    console.log(
      'Error occurred in searchCapability capability service : ',
      error
    );
    throw error;
  }
};

export {
  createCapability,
  updateCapability,
  getAllCapabilities,
  getById,
  deleteCapability,
  searchCapability,
};
