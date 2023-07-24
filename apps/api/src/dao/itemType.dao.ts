import prisma from '../utils/prisma';

const add = async (
  item_type_item_code: string,
  item_type_item_name: string,
  created_by: bigint,
  updated_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const itemType = await transaction.item_type.create({
      data: {
        item_type_item_code,
        item_type_item_name,
        created_by,
        updated_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return itemType;
  } catch (error) {
    console.log('Error occurred in itemTypeDao add dao', error);
    throw error;
  }
};
const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const itemTypes = await transaction.item_type.findMany({});
    return itemTypes;
  } catch (error) {
    console.log('Error occurred in itemType getAll dao', error);
    throw error;
  }
};
const getById = async (item_type_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const itemType = await transaction.item_type.findUnique({
      where: {
        item_type_id: Number(item_type_id),
      },
    });
    return itemType;
  } catch (error) {
    console.log('Error occurred in itemType getById dao', error);
    throw error;
  }
};
const deleteItemType = async (item_type_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const itemType = await transaction.item_type.delete({
      where: {
        item_type_id: Number(item_type_id),
      },
    });
    return itemType;
  } catch (error) {
    console.log('Error occurred in itemType delete dao', error);
    throw error;
  }
};
const edit = async (
  item_type_id: number,
  item_type_item_code: string,
  item_type_item_name: string,
  updated_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const itemType = await transaction.item_type.update({
      where: {
        item_type_id: item_type_id,
      },
      data: {
        item_type_id,
        item_type_item_code,
        item_type_item_name,
        updated_by,
        updated_date: currentDate,
      },
    });
    return itemType;
  } catch (error) {
    console.log('Error occurred in itemTypeDao edit', error);
    throw error;
  }
};

export default {
  add,
  getAll,
  getById,
  deleteItemType,
  edit,
};
