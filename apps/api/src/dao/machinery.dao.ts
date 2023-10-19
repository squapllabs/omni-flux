import prisma from '../utils/prisma';

const add = async (
  machinery_name: string,
  machinery_model: string,
  machinery_type: string,
  manufacturer: string,
  date_of_purchase: Date,
  warranty_expired_on: Date,
  operational_status: string,
  location: string,
  rate: number,
  uom_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_date_of_purchase = date_of_purchase
      ? new Date(date_of_purchase)
      : null;
    const formatted_warranty_expired_on = warranty_expired_on
      ? new Date(warranty_expired_on)
      : null;
    const is_delete = false;
    const transaction = connectionObj ? connectionObj : prisma;
    const machinery = await transaction.machinery.create({
      data: {
        machinery_name,
        machinery_model,
        machinery_type,
        manufacturer,
        date_of_purchase: formatted_date_of_purchase,
        warranty_expired_on: formatted_warranty_expired_on,
        operational_status,
        location,
        rate,
        uom_id,
        is_delete: is_delete,
        created_date: currentDate,
        updated_date: currentDate,
        created_by,
      },
    });
    return machinery;
  } catch (error) {
    console.log('Error occurred in machinery dao : add', error);
    throw error;
  }
};

const edit = async (
  machinery_id: number,
  machinery_name: string,
  machinery_model: string,
  machinery_type: string,
  manufacturer: string,
  date_of_purchase: Date,
  warranty_expired_on: Date,
  operational_status: string,
  location: string,
  rate: number,
  uom_id: number,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_date_of_purchase = date_of_purchase
      ? new Date(date_of_purchase)
      : null;
    const formatted_warranty_expired_on = warranty_expired_on
      ? new Date(warranty_expired_on)
      : null;
    const transaction = connectionObj ? connectionObj : prisma;
    const machinery = await transaction.machinery.update({
      where: {
        machinery_id: machinery_id,
      },
      data: {
        machinery_name,
        machinery_model,
        machinery_type,
        manufacturer,
        date_of_purchase: formatted_date_of_purchase,
        warranty_expired_on: formatted_warranty_expired_on,
        operational_status,
        location,
        rate,
        uom_id,
        updated_date: currentDate,
        updated_by,
      },
    });
    return machinery;
  } catch (error) {
    console.log('Error occurred in machinery dao : edit', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const machinery = await transaction.machinery.findMany({
      where: {
        is_delete: false,
      },
      include: { uom_data: true },
      orderBy: [{ updated_date: 'desc' }],
    });
    return machinery;
  } catch (error) {
    console.log('Error occurred in machinery dao : getAll', error);
    throw error;
  }
};

const getById = async (machinery_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const machinery = await transaction.machinery.findFirst({
      where: {
        machinery_id: Number(machinery_id),
        is_delete: false,
      },
      include: { uom_data: true },
    });
    return machinery;
  } catch (error) {
    console.log('Error occurred in machinery dao : getById', error);
    throw error;
  }
};

const deleteById = async (machinery_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const machinery = await transaction.machinery.update({
      where: {
        machinery_id: Number(machinery_id),
      },
      data: {
        is_delete: true,
      },
    });
    return machinery;
  } catch (error) {
    console.log('Error occurred in machinery dao : deleteById', error);
    throw error;
  }
};

const searchMachinery = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterMachinery;
    const getData = await transaction.machinery.findMany({
      where: {
        is_delete: filter.is_delete,
      },
    });
    if (getData.length > 0) {
      const machinery = await transaction.machinery.findMany({
        where: filter,
        include: { uom_data: true },
        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
      const machineryCount = await transaction.machinery.count({
        where: filter,
      });
      const machineryData = {
        count: machineryCount,
        data: machinery,
      };
      return machineryData;
    } else {
      return getData;
    }
  } catch (error) {
    console.log('Error occurred in Machinery dao : searchMachinery ', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getAll,
  getById,
  deleteById,
  searchMachinery,
};
