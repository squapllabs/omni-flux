import prisma from '../utils/prisma';

const add = async (
  capability_name: string,
  description: string,
  permission_type: string,
  ui_type: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj ? connectionObj : prisma;
    const capability = await transaction.capability.create({
      data: {
        capability_name,
        description,
        permission_type,
        ui_type,
        is_delete: is_delete,
        created_date: currentDate,
        updated_date: currentDate,
        created_by,
      },
    });
    return capability;
  } catch (error) {
    console.log('Error occurred in capability dao : add', error);
    throw error;
  }
};

const edit = async (
  capability_name: string,
  description: string,
  permission_type: string,
  ui_type: string,
  updated_by: number,
  capability_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj ? connectionObj : prisma;
    const capability = await transaction.capability.update({
      where: {
        capability_id: capability_id,
      },
      data: {
        capability_name,
        description,
        permission_type,
        ui_type,
        updated_date: currentDate,
        updated_by,
      },
    });
    return capability;
  } catch (error) {
    console.log('Error occurred in capability dao : edit', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const capability = await transaction.capability.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return capability;
  } catch (error) {
    console.log('Error occurred in capability dao : getAll', error);
    throw error;
  }
};

const getById = async (capability_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const capability = await transaction.capability.findFirst({
      where: {
        capability_id: Number(capability_id),
        is_delete: false,
      },
    });
    return capability;
  } catch (error) {
    console.log('Error occurred in capability dao : getById', error);
    throw error;
  }
};

const deleteById = async (capability_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const capability = await transaction.capability.update({
      where: {
        capability_id: Number(capability_id),
      },
      data: {
        is_delete: true,
      },
    });
    return capability;
  } catch (error) {
    console.log('Error occurred in capability dao : deleteById', error);
    throw error;
  }
};

const searchCapability = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterCapability;
    const getCapabilityData = await transaction.capability.findMany({});
    if (getCapabilityData?.length > 0) {
      const capability = await transaction.capability.findMany({
        where: filter,
        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
      const capabilityCount = await transaction.capability.count({
        where: filter,
      });
      const capabilityData = {
        count: capabilityCount,
        data: capability,
      };
      return capabilityData;
    } else {
      return getCapabilityData;
    }
  } catch (error) {
    console.log('Error occurred in capability dao : searchCapability ', error);
    throw error;
  }
};

export default { add, edit, getAll, getById, deleteById, searchCapability };
