import prisma from '../utils/prisma';

const add = async (
  item_name: string,
  item_category: string,
  rate: number,
  available_quantity: number,
  store_id: number,
  project_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const inventory = await transaction.inventory.create({
      data: {
        item_name,
        item_category,
        rate,
        available_quantity,
        store_id,
        project_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return inventory;
  } catch (error) {
    console.log('Error occurred in inventoryDao add', error);
    throw error;
  }
};

const edit = async (
  item_name: string,
  item_category: string,
  rate: number,
  available_quantity: number,
  store_id: number,
  project_id: number,
  updated_by: number,
  inventory_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const inventory = await transaction.inventory.update({
      where: {
        inventory_id: inventory_id,
      },
      data: {
        item_name,
        item_category,
        rate,
        available_quantity,
        store_id,
        project_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return inventory;
  } catch (error) {
    console.log('Error occurred in inventoryDao edit', error);
    throw error;
  }
};

const getById = async (inventoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const inventory = await transaction.inventory.findFirst({
      where: {
        inventory_id: Number(inventoryId),
        is_delete: false,
      },
      include: {
        store_data: {
          include: {
            store_manager_data: {
              select: { first_name: true, last_name: true },
            },
          },
        },
        project_data: true,
      },
    });
    return inventory;
  } catch (error) {
    console.log('Error occurred in inventory getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const inventory = await transaction.inventory.findMany({
      where: {
        is_delete: false,
      },
      include: {
        store_data: {
          include: {
            store_manager_data: {
              select: { first_name: true, last_name: true },
            },
          },
        },
        project_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return inventory;
  } catch (error) {
    console.log('Error occurred in inventory getAll dao', error);
    throw error;
  }
};

const deleteInventory = async (inventoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const inventory = await transaction.inventory.update({
      where: {
        inventory_id: Number(inventoryId),
      },
      data: {
        is_delete: true,
      },
    });
    return inventory;
  } catch (error) {
    console.log('Error occurred in inventory deleteInventory dao', error);
    throw error;
  }
};

const searchInventory = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterInventory;
    const inventory = await transaction.inventory.findMany({
      where: filter,
      include: {
        store_data: {
          include: {
            store_manager_data: {
              select: { first_name: true, last_name: true },
            },
          },
        },
        project_data: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const inventoryCount = await transaction.inventory.count({
      where: filter,
    });
    const inventoryData = {
      count: inventoryCount,
      data: inventory,
    };
    return inventoryData;
  } catch (error) {
    console.log('Error occurred in inventory dao : searchInventory', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteInventory,
  searchInventory,
};
