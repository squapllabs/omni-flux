import permissionDao from '../dao/permissions.dao';
import roleDao from '../dao/role.dao';
import capabilityDao from '../dao/capability.dao';
import { permissionsBody } from '../interfaces/permissions.Interface';

/**
 * Method to Create a New Permission
 * @param body
 * @returns
 */
const createPermission = async (body: permissionsBody) => {
  try {
    const {
      is_create,
      is_update,
      is_read,
      is_visible,
      capability_id,
      role_id,
      created_by,
    } = body;
    let result = null;

    if (capability_id) {
      const capabilityExist = await capabilityDao.getById(capability_id);
      if (!capabilityExist) {
        result = {
          message: 'capability_id does not exist',
          status: false,
          data: capabilityExist,
        };
        return result;
      }
    }

    if (role_id) {
      const roleExist = await roleDao.getById(role_id);
      if (!roleExist) {
        result = {
          message: 'role_id does not exist',
          status: false,
          data: roleExist,
        };
        return result;
      }
    }

    if (role_id && capability_id) {
      const capabilityRoleExist =
        await permissionDao.getByRoleIdAndCapabilityId(role_id, capability_id);
      if (capabilityRoleExist) {
        result = {
          message: 'This role_id and capability_id combination already exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const permissionDetails = await permissionDao.add(
      is_create,
      is_update,
      is_read,
      is_visible,
      capability_id,
      role_id,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: permissionDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in permission service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Permission
 * @param body
 * @returns
 */
const updatePermission = async (body: permissionsBody) => {
  try {
    const {
      is_create,
      is_update,
      is_read,
      is_visible,
      capability_id,
      role_id,
      updated_by,
      permission_id,
    } = body;
    let result = null;

    const permissionExist = await permissionDao.getById(permission_id);
    if (!permissionExist) {
      result = {
        message: 'permission_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (capability_id) {
      const capabilityExist = await capabilityDao.getById(capability_id);
      if (!capabilityExist) {
        result = {
          message: 'capability_id does not exist',
          status: false,
          data: capabilityExist,
        };
        return result;
      }
    }

    if (role_id) {
      const roleExist = await roleDao.getById(role_id);
      if (!roleExist) {
        result = {
          message: 'role_id does not exist',
          status: false,
          data: roleExist,
        };
        return result;
      }
    }

    if (role_id && capability_id) {
      const capabilityRoleExist =
        await permissionDao.getByRoleIdAndCapabilityId(role_id, capability_id);

      if (capabilityRoleExist) {
        if (capabilityRoleExist.permission_id !== permission_id) {
          result = {
            message: 'This role_id and capability_id combination already exist',
            status: false,
            data: null,
          };
          return result;
        }
      }
    }

    const permissionDetails = await permissionDao.edit(
      is_create,
      is_update,
      is_read,
      is_visible,
      capability_id,
      role_id,
      updated_by,
      permission_id
    );
    result = {
      message: 'success',
      status: true,
      data: permissionDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in permission service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Permission By permissionId
 * @param permissionId
 * @returns
 */
const getById = async (permissionId: number) => {
  try {
    let result = null;
    const permissionData = await permissionDao.getById(permissionId);
    if (permissionData) {
      result = { message: 'success', status: true, data: permissionData };
      return result;
    } else {
      result = {
        status: false,
        message: 'permission_id does not exist',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById permission service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Permission's
 * @returns
 */
const getAllPermissions = async () => {
  try {
    const result = await permissionDao.getAll();
    const permissionData = { message: 'success', status: true, data: result };
    return permissionData;
  } catch (error) {
    console.log(
      'Error occurred in getAllPermissions permission service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete permission
 * @param permissionId
 */
const deletePermission = async (permissionId: number) => {
  try {
    const permissionExist = await permissionDao.getById(permissionId);

    if (!permissionExist) {
      const result = {
        status: false,
        message: 'permission_id does not exist',
        data: null,
      };
      return result;
    }

    const data = await permissionDao.deleteById(permissionId);
    if (data) {
      const result = {
        status: true,
        message: 'Permission Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this permission',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deletePermission permission service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Permission - Pagination API
 * @returns
 */
const searchPermission = async (body) => {
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
      filterObj.filterPermission = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (global_search) {
      filterObj.filterPermission = filterObj.filterPermission || {};
      filterObj.filterPermission.OR = filterObj.filterPermission.OR || [];

      filterObj.filterPermission.OR.push(
        {
          role_data: {
            role_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          capability_data: {
            capability_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await permissionDao.searchPermission(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempLeadEnquiryData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempLeadEnquiryData;
  } catch (error) {
    console.log(
      'Error occurred in searchPermission permission service : ',
      error
    );
    throw error;
  }
};

export {
  createPermission,
  updatePermission,
  getAllPermissions,
  getById,
  deletePermission,
  searchPermission,
};
