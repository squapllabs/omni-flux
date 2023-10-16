import prisma from '../utils/prisma';

const add = async (
  name: string,
  description: string,
  created_by: bigint,
  uom_type: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom = await transaction.uom.create({
      data: {
        name,
        description,
        created_by,
        uom_type,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return uom;
  } catch (error) {
    console.log('Error occurred in uomDao add', error);
    throw error;
  }
};

const edit = async (
  name: string,
  description: string,
  updated_by: bigint,
  uom_type: string,
  uom_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom = await transaction.uom.update({
      where: {
        uom_id: uom_id,
      },
      data: {
        name,
        description,
        updated_by,
        uom_type,
        updated_date: currentDate,
      },
    });
    return uom;
  } catch (error) {
    console.log('Error occurred in uomDao edit', error);
    throw error;
  }
};

const getById = async (uomId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom = await transaction.uom.findFirst({
      where: {
        uom_id: Number(uomId),
        is_delete: false,
      },
    });
    return uom;
  } catch (error) {
    console.log('Error occurred in uom getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom = await transaction.uom.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return uom;
  } catch (error) {
    console.log('Error occurred in uom getAll dao', error);
    throw error;
  }
};

const deleteUom = async (uomId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom = await transaction.uom.delete({
      where: {
        uom_id: Number(uomId),
      },
    });
    return uom;
  } catch (error) {
    console.log('Error occurred in uom deleteUom dao', error);
    throw error;
  }
};

const getByName = async (name: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom =
      await transaction.$queryRaw`select * from uom u where lower(u."name") = lower(${name})`;
    return uom;
  } catch (error) {
    console.log('Error occurred in uom getByName dao', error);
    throw error;
  }
};

const searchUOM = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterUom;
    const getdata = await transaction.uom.findMany({
      where: {
        is_delete: false,
      },
    });
    if (getdata.length != 0) {
      const uom = await transaction.uom.findMany({
        where: filter,
        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
      const uomCount = await transaction.uom.count({
        where: filter,
      });
      const uomData = {
        count: uomCount,
        data: uom,
      };
      return uomData;
    } else {
      return false;
    }
  } catch (error) {
    console.log('Error occurred in uom dao : searchUOM ', error);
    throw error;
  }
};

const getByType = async (uomType: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom = await transaction.uom.findMany({
      where: {
        uom_type: uomType,
        is_delete: false,
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return uom;
  } catch (error) {
    console.log('Error occurred in uom getByType dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteUom,
  getByName,
  searchUOM,
  getByType,
};
