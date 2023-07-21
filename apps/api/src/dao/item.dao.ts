import prisma from '../utils/prisma';
import { createItemBody } from '../interfaces/item.interface';

const add = async (
  item_name: string,
  sub_sub_category_id: number,
  description: string,
  hsn_code_id: number,
  gst_id: number,
  uom_id: number,
  created_by: bigint,
  updated_by: bigint,
  item_type_id: number,
  brand_id:number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.create({
      data: {
        item_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
        item_type_id,
        created_date: currentDate,
        updated_date: currentDate,
        brand_id
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in itemDao add dao', error);
    throw error;
  }
};
// item.dao.ts

const addBulk = async (items: createItemBody[], connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const createdItems = await transaction.item.createMany({
      data: items
    });

    return createdItems;
  } catch (error) {
    console.log('Error occurred in itemDao addBulk dao', error);
    throw error;
  }
};
const getAll = async (
  offset,
  limit,
  orderByColumn,
  orderByDirection,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterItem;
    const items = await transaction.item.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
      include: {
        gst: {
          select: {
            rate: true,
            gst_id: true,
          },
        },
        hsn_code: {
          select: {
            code: true,
            hsn_code_id: true,
          },
        },
        uom: {
          select: {
            name: true,
            uom_id: true,
          },
        },
        sub_sub_category: {
          select: {
            name: true,
            sub_sub_category_id: true,
          },
        },
        item_type: {
          select: {
            item_type_item_name: true,
          },
        },
        brand: {
          select: {
            brand_name: true,
          },
        },
      },
    });
    return items;
  } catch (error) {
    console.log('Error occurred in item getAll dao', error);
    throw error;
  }
};

const getAllBySearch = async (keyword, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const items = await transaction.item.findMany({
      where: {
        OR: [
          {
            description: {
              contains: keyword,
              mode: 'insensitive',
            },
          },
          {
            item_name: {
              contains: keyword,
              mode: 'insensitive',
            },
          },
        ],
      },
    });

    return items;
  } catch (error) {
    console.log('Error occurred in item getAll dao', error);
    throw error;
  }
};

const getById = async (item_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.findUnique({
      where: {
        item_id: Number(item_id),
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in item getById dao', error);
    throw error;
  }
};
const deleteItem = async (item_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.delete({
      where: {
        item_id: Number(item_id),
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in item delete dao', error);
    throw error;
  }
};
const edit = async (
  item_id: number,
  item_name: string,
  sub_sub_category_id: number,
  description: string,
  hsn_code_id: number,
  gst_id: number,
  uom_id: number,
  updated_by: bigint,
  item_type_id:number,
  brand_id:number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.update({
      where: {
        item_id: item_id,
      },
      data: {
        item_name,
        sub_sub_category_id,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        updated_by,
        updated_date: currentDate,
        item_type_id,
        brand_id
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in itemDao edit', error);
    throw error;
  }
};
export default {
  add,
  getAll,
  getById,
  deleteItem,
  edit,
  addBulk,
  getAllBySearch,
};
