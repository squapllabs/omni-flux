import prisma from '../utils/prisma';

const add = async (
  warehouse_id: number,
  product_id: number,
  quantity: number,
  item_id: number,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouseInventory = await transaction.warehouse_inventory.create({
      data: {
        warehouse_id,
        product_id,
        quantity,
        item_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return warehouseInventory;
  } catch (error) {
    console.log('Error occurred in warehouseInventoryDao add', error);
    throw error;
  }
};

const edit = async (
  warehouse_id: number,
  product_id: number,
  quantity: number,
  item_id: number,
  updated_by: bigint,
  warehouse_inventory_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouseInventory = await transaction.warehouse_inventory.update({
      where: {
        warehouse_inventory_id: warehouse_inventory_id,
      },
      data: {
        warehouse_id,
        product_id,
        quantity,
        item_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return warehouseInventory;
  } catch (error) {
    console.log('Error occurred in warehouseInventoryDao edit', error);
    throw error;
  }
};

const getById = async (warehouseInventoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouseInventory = await transaction.warehouse_inventory.findUnique(
      {
        where: {
          warehouse_inventory_id: Number(warehouseInventoryId),
        },
      }
    );
    return warehouseInventory;
  } catch (error) {
    console.log('Error occurred in warehouseInventory getById dao', error);
    throw error;
  }
};

const checkDuplicateWarehouseIdAndItemId = async (
  warehouseId: number,
  itemId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouseInventory = await transaction.warehouse_inventory.findFirst({
      where: {
        warehouse_id: Number(warehouseId),
        item_id: Number(itemId),
      },
    });
    return warehouseInventory;
  } catch (error) {
    console.log(
      'Error occurred in warehouseInventory checkDuplicateWarehouseIdAndItemId dao',
      error
    );
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouseInventory = await transaction.warehouse_inventory.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return warehouseInventory;
  } catch (error) {
    console.log('Error occurred in warehouseInventory getAll dao', error);
    throw error;
  }
};

const deleteWarehouseInventory = async (
  warehouseInventoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouseInventory = await transaction.warehouse_inventory.delete({
      where: {
        warehouse_inventory_id: Number(warehouseInventoryId),
      },
    });
    return warehouseInventory;
  } catch (error) {
    console.log(
      'Error occurred in warehouseInventory deleteWarehouseInventory dao',
      error
    );
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  checkDuplicateWarehouseIdAndItemId,
  getAll,
  deleteWarehouseInventory,
};
