import prisma from '../utils/prisma';

const add = async (
  rate: number,
  cgst_rate: number,
  igst_rate: number,
  sgst_rate: number,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const gst = await transaction.gst.create({
      data: {
        rate,
        cgst_rate,
        igst_rate,
        sgst_rate,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return gst;
  } catch (error) {
    console.log('Error occurred in gstDao add', error);
    throw error;
  }
};

const edit = async (
  rate: number,
  cgst_rate: number,
  igst_rate: number,
  sgst_rate: number,
  updated_by: bigint,
  gst_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const gst = await transaction.gst.update({
      where: {
        gst_id: gst_id,
      },
      data: {
        rate,
        cgst_rate,
        igst_rate,
        sgst_rate,
        updated_by,
        updated_date: currentDate,
      },
    });
    return gst;
  } catch (error) {
    console.log('Error occurred in gstDao edit', error);
    throw error;
  }
};

const getById = async (gstId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const gst = await transaction.gst.findFirst({
      where: {
        gst_id: Number(gstId),
        is_delete: false,
      },
    });
    return gst;
  } catch (error) {
    console.log('Error occurred in gst getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const gst = await transaction.gst.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return gst;
  } catch (error) {
    console.log('Error occurred in gst getAll dao', error);
    throw error;
  }
};

const deleteGst = async (gstId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const gst = await transaction.gst.update({
      where: {
        gst_id: Number(gstId),
      },
      data: {
        is_delete: true,
      },
    });
    return gst;
  } catch (error) {
    console.log('Error occurred in gst deleteGst dao', error);
    throw error;
  }
};

const searchGST = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterGst;
    const getdata = await transaction.gst.findMany({
      where: {
        is_delete: false,
      },
    });
    if (getdata.length != 0) {
      const gst = await transaction.gst.findMany({
        where: filter,
        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
      const gstCount = await transaction.gst.count({
        where: filter,
      });
      const gstData = {
        count: gstCount,
        data: gst,
      };
      return gstData;
    } else {
      return false;
    }
  } catch (error) {
    console.log('Error occurred in gst dao : searchGST ', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteGst,
  searchGST,
};
