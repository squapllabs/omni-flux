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
    const gst = await transaction.gst.findUnique({
      where: {
        gst_id: Number(gstId),
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
    const gst = await transaction.gst.delete({
      where: {
        gst_id: Number(gstId),
      },
    });
    return gst;
  } catch (error) {
    console.log('Error occurred in gst deleteGst dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteGst,
};
