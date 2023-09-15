import prisma from '../utils/prisma';

const add = async (
  is_create: boolean,
  is_update: boolean,
  is_read: boolean,
  is_visible: boolean,
  capability_id: number,
  role_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj ? connectionObj : prisma;
    const permission = await transaction.permissions.create({
      data: {
        is_create,
        is_update,
        is_read,
        is_visible,
        capability_id,
        role_id,
        is_delete: is_delete,
        created_date: currentDate,
        updated_date: currentDate,
        created_by,
      },
    });
    return permission;
  } catch (error) {
    console.log('Error occurred in permission dao : add', error);
    throw error;
  }
};

const edit = async (
  is_create: boolean,
  is_update: boolean,
  is_read: boolean,
  is_visible: boolean,
  capability_id: number,
  role_id: number,
  updated_by: number,
  permission_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj ? connectionObj : prisma;
    const permission = await transaction.permissions.update({
      where: {
        permission_id: permission_id,
      },
      data: {
        is_create,
        is_update,
        is_read,
        is_visible,
        capability_id,
        role_id,
        updated_date: currentDate,
        updated_by,
      },
    });
    return permission;
  } catch (error) {
    console.log('Error occurred in permission dao : edit', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    console.log('Value of transaction:');
    const permission = await transaction.permissions.findMany({
      where: {
        is_delete: false,
      },
      include: {
        role_data: true,
        capability_data: true,
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return permission;
  } catch (error) {
    console.log('Error occurred in permission dao : getAll', error);
    throw error;
  }
};

const getById = async (permission_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const permission = await transaction.permissions.findFirst({
      where: {
        permission_id: Number(permission_id),
        is_delete: false,
      },
      include: {
        role_data: true,
        capability_data: true,
      },
    });
    return permission;
  } catch (error) {
    console.log('Error occurred in permission dao : getById', error);
    throw error;
  }
};

const getByRoleIdAndCapabilityId = async (
  role_id: number,
  capability_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const permission = await transaction.permissions.findFirst({
      where: {
        role_id: Number(role_id),
        capability_id: Number(capability_id),
        is_delete: false,
      },
    });
    return permission;
  } catch (error) {
    console.log(
      'Error occurred in permission dao : getByRoleIdAndCapabilityId',
      error
    );
    throw error;
  }
};

const deleteById = async (permission_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const permission = await transaction.permissions.update({
      where: {
        permission_id: Number(permission_id),
      },
      data: {
        is_delete: true,
      },
    });
    return permission;
  } catch (error) {
    console.log('Error occurred in permission dao : deleteById', error);
    throw error;
  }
};

const getByCapabilityId = async (
  capability_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const permission = await transaction.permissions.findFirst({
      where: {
        capability_id: Number(capability_id),
        is_delete: false,
      },
      include: {
        role_data: true,
        capability_data: true,
      },
    });
    return permission;
  } catch (error) {
    console.log('Error occurred in permission dao : getByCapabilityId', error);
    throw error;
  }
};

const searchPermission = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterPermission;
    const permission = await transaction.permissions.findMany({
      where: filter,
      include: {
        role_data: true,
        capability_data: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const permissionCount = await transaction.permissions.count({
      where: filter,
    });
    const permissionData = {
      count: permissionCount,
      data: permission,
    };
    return permissionData;
  } catch (error) {
    console.log('Error occurred in permission dao : searchPermission ', error);
    throw error;
  }
};

const getByUserId = async (User_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const permission = await transaction.permissions.findMany({
      where: {
        role_data: {
          user_roles: {
            some: {
              user_data: {
                user_id: Number(User_id),
              }
            }
          }
        },
        capability_data: {
          permission_type: 'project'
        }
      }
    });
    console.log(permission);
    return permission;
  } catch (error) {
    console.log('Error occurred in permission dao : getById', error);
    throw error;
  }
};

export default {
  getByUserId,
  add,
  edit,
  getAll,
  getById,
  getByRoleIdAndCapabilityId,
  deleteById,
  getByCapabilityId,
  searchPermission,
};
