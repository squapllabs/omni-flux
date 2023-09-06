import prisma from '../utils/prisma';

const add = async (
  labour_type: string,
  quantity: number,
  rate: number,
  total: number,
  uom_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const labour = await transaction.labour.create({
      data: {
        labour_type,
        quantity,
        rate,
        total,
        uom_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return labour;
  } catch (error) {
    console.log('Error occurred in labourDao add', error);
    throw error;
  }
};

const edit = async (
  labour_id: number,
  labour_type: string,
  quantity: number,
  rate: number,
  total: number,
  uom_id: number,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const labour = await transaction.labour.update({
      where: {
        labour_id: labour_id,
      },
      data: {
        labour_type,
        quantity,
        rate,
        total,
        uom_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return labour;
  } catch (error) {
    console.log('Error occurred in labourDao edit', error);
    throw error;
  }
};

const getById = async (labourId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const labour = await transaction.labour.findFirst({
      where: {
        labour_id: Number(labourId),
        is_delete: false,
      },
      include: {
        uom: true,
      },
    });
    return labour;
  } catch (error) {
    console.log('Error occurred in labour getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const labour = await transaction.labour.findMany({
      where: {
        is_delete: false,
      },
      include: {
        uom: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return labour;
  } catch (error) {
    console.log('Error occurred in labour getAll dao', error);
    throw error;
  }
};

const deleteLabour = async (labourId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const labour = await transaction.labour.update({
      where: {
        labour_id: Number(labourId),
      },
      data: {
        is_delete: true,
      },
    });
    return labour;
  } catch (error) {
    console.log('Error occurred in labour deleteLabour dao', error);
    throw error;
  }
};

const searchLabour = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterLabour;
    const labour = await transaction.labour.findMany({
      where: filter,
      include: {
        uom: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const labourCount = await transaction.labour.count({
      where: filter,
    });
    const labourData = {
      count: labourCount,
      data: labour,
    };
    return labourData;
  } catch (error) {
    console.log('Error occurred in labour dao : searchLabour', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteLabour,
  searchLabour,
};
