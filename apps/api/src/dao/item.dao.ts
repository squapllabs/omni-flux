import prisma from '../utils/prisma';
import { createItemBody } from '../interfaces/item.interface';

const add = async (
  item_name: string,
  description: string,
  hsn_code_id: number,
  gst_id: number,
  uom_id: number,
  created_by: bigint,
  updated_by: bigint,
  item_type_id: number,
  brand_id: number,
  rate: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.create({
      data: {
        item_name,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        created_by,
        updated_by,
        item_type_id,
        created_date: currentDate,
        updated_date: currentDate,
        brand_id,
        is_delete: is_delete,
        rate,
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in itemDao add dao', error);
    throw error;
  }
};

const addBulk = async (items: createItemBody[], connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const createdItems = await transaction.item.createMany({
      data: items,
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
    const totalCount = await transaction.item.count({});

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
    return { totalCount, items };
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
    const item = await transaction.item.findFirst({
      where: {
        item_id: Number(item_id),
        is_delete: false,
      },
      include: {
        gst: {
          select: {
            rate: true,
          },
        },
        hsn_code: {
          select: {
            code: true,
          },
        },
        uom: {
          select: {
            name: true,
          },
        },
        item_type: {
          select: {
            master_data_name: true,
          },
        },
        brand: {
          select: {
            brand_name: true,
          },
        },
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
    const item = await transaction.item.update({
      where: {
        item_id: Number(item_id),
      },
      data: {
        is_delete: true,
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
  description: string,
  hsn_code_id: number,
  gst_id: number,
  uom_id: number,
  updated_by: bigint,
  item_type_id: number,
  brand_id: number,
  rate: number,
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
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        updated_by,
        updated_date: currentDate,
        item_type_id,
        brand_id,
        rate,
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in itemDao edit', error);
    throw error;
  }
};

const getByHSNCodeId = async (hsn_code_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.findFirst({
      where: {
        hsn_code_id: Number(hsn_code_id),
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in item getByHSNCodeId dao', error);
    throw error;
  }
};

const getByGSTId = async (gstId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.findFirst({
      where: {
        gst_id: Number(gstId),
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in item getByGSTId dao', error);
    throw error;
  }
};

const getByUOMId = async (uomId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.findFirst({
      where: {
        uom_id: Number(uomId),
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in item getByUOMId dao', error);
    throw error;
  }
};

const getAllItems = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.findMany({
      where: { is_delete: false },
      orderBy: [{ updated_date: 'desc' }],
      include: {
        gst: {
          select: {
            rate: true,
          },
        },
        hsn_code: {
          select: {
            code: true,
          },
        },
        uom: {
          select: {
            name: true,
          },
        },
        item_type: {
          select: {
            master_data_name: true,
          },
        },
        brand: {
          select: {
            brand_name: true,
          },
        },
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in item getAllItems dao', error);
    throw error;
  }
};

const searchItem = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterItem;
    const getdata = await transaction.item.findMany({
      where: {
        is_delete: false,
      },
    });
    if (getdata.length != 0) {
      const item = await transaction.item.findMany({
        where: filter,
        include: {
          gst: {
            select: {
              rate: true,
            },
          },
          hsn_code: {
            select: {
              code: true,
            },
          },
          uom: {
            select: {
              name: true,
            },
          },
          item_type: {
            select: {
              master_data_name: true,
            },
          },
          brand: {
            select: {
              brand_name: true,
            },
          },
        },
        orderBy: [
          {
            [orderByColumn]: orderByDirection,
          },
        ],
        skip: offset,
        take: limit,
      });
      const itemCount = await transaction.item.count({
        where: filter,
      });
      const itemData = {
        count: itemCount,
        data: item,
      };
      return itemData;
    } else {
      return false;
    }
  } catch (error) {
    console.log('Error occurred in item dao : searchItem ', error);
    throw error;
  }
};

const getByItemName = async (item_name: string, connectionObj = null) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const item = await transaction.item.findFirst({
      where: {
        item_name: {
          equals: item_name,
          mode: 'insensitive',
        },
      },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in Item Dao : getByItemName ', error);
    throw error;
  }
};

const getByIndentRequestId = async (
  indent_request_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const item = await transaction.item.findMany({
      where: {
        bom_detail: {
          some: {
            indent_request_details: {
              some: { indent_request_id: Number(indent_request_id) },
            },
          },
        },
        is_delete: false,
      },
      include: {
        bom_detail: {
          select: { quantity: true },
          where: {
            indent_request_details: {
              some: { indent_request_id: Number(indent_request_id) },
            },
          },
        },
      },
      orderBy: { updated_date: 'desc' },
    });
    return item;
  } catch (error) {
    console.log('Error occurred in item getByIndentRequestId dao', error);
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
  getByHSNCodeId,
  getByGSTId,
  getByUOMId,
  getAllItems,
  searchItem,
  getByItemName,
  getByIndentRequestId,
};
