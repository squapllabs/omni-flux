import prisma from '../utils/prisma';

const add = async (
  code: string,
  description: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.create({
      data: {
        code,
        description,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnDao add', error);
    throw error;
  }
};

const edit = async (
  code: string,
  description: string,
  updated_by: bigint,
  hsn_code_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.update({
      where: {
        hsn_code_id: hsn_code_id,
      },
      data: {
        code,
        description,
        updated_by,
        updated_date: currentDate,
      },
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnDao edit', error);
    throw error;
  }
};

const getById = async (hsnCodeId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.findUnique({
      where: {
        hsn_code_id: Number(hsnCodeId),
      },
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnCode getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnCode getAll dao', error);
    throw error;
  }
};

const deleteHsnCode = async (hsnCodeId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.delete({
      where: {
        hsn_code_id: Number(hsnCodeId),
      },
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnCode deleteHsnCode dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteHsnCode,
};
