import prisma from '../utils/prisma';

const add = async (
  code: string,
  description: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.create({
      data: {
        code,
        description,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
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
    const hsnCode = await transaction.hsn_code.findFirst({
      where: {
        hsn_code_id: Number(hsnCodeId),
        is_delete: false,
      },
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnCode getById dao', error);
    throw error;
  }
};

const getByCode = async (code: string, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.findUnique({
      where: {
        code: code,
      },
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnCode getByCode dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.findMany({
      where: {
        is_delete: false,
      },
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
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const hsnCode = await transaction.hsn_code.update({
      where: {
        hsn_code_id: Number(hsnCodeId),
      },
      data: {
        is_delete: true,
        updated_date: currentDate,
      },
    });
    return hsnCode;
  } catch (error) {
    console.log('Error occurred in hsnCode deleteHsnCode dao', error);
    throw error;
  }
};

const addBulk = async (hsnCodes, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    let successCount = 0;
    const createdData = await Promise.all(
      hsnCodes.map(async (codeData) => {
        const { code, ...rest } = codeData;
        try {
          const createdRecord = await transaction.hsn_code.create({
            data: {
              code,
              ...rest,
            },
          });
          successCount++;
          return createdRecord;
        } catch (error) {
          if (error.code === 'P2002') {
            const uniqueValidation = {
              unique: `This code ${code} already exist`,
            };
            return uniqueValidation;
          }
          console.error('Error occurred for data:', codeData, error);
          throw error;
        }
      })
    );

    return {
      createdData,
      successCount,
    };
  } catch (error) {
    console.error('Error occurred in hsnCode addBulk dao', error);
    throw error;
  }
};

const searchHSNCode = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterHSNCode;
    const hsnCode = await transaction.hsn_code.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const hsnCodeCount = await transaction.hsn_code.count({
      where: filter,
    });
    const hsnCodeData = {
      count: hsnCodeCount,
      data: hsnCode,
    };
    return hsnCodeData;
  } catch (error) {
    console.log('Error occurred in hsnCode dao : searchHSNCode ', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getByCode,
  getAll,
  deleteHsnCode,
  addBulk,
  searchHSNCode,
};
