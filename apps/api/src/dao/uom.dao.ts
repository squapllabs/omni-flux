import prisma from '../utils/prisma';

const add = async (
  name: string,
  description: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const uom = await transaction.uom.create({
      data: {
        name,
        description,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
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
    const uom = await transaction.uom.findUnique({
      where: {
        uom_id: Number(uomId),
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

export default {
  add,
  edit,
  getById,
  getAll,
  deleteUom,
};
