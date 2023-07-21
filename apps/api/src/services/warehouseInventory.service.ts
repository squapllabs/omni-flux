import warehouseInventoryDao from '../dao/warehouseInventory.dao';
import warehouseDao from '../dao/warehouse.dao';
import {
  createWarehouseInventoryBody,
  updateWarehouseInventoryBody,
} from '../interfaces/warehouseInventory.Interface';

/**
 * Method to Create a New warehouseInventory
 * @param body
 * @returns
 */
const createWarehouseInventory = async (body: createWarehouseInventoryBody) => {
  try {
    const { warehouse_id, product_id, quantity, item_id, created_by } = body;
    let result = null;
    const warehouseExist = await warehouseDao.getById(warehouse_id);
    if (!warehouseExist) {
      result = {
        message: 'warehouse_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    if (warehouse_id && item_id) {
      const checkDuplicateWarehouseAndItem =
        await warehouseInventoryDao.checkDuplicateWarehouseIdAndItemId(
          warehouse_id,
          item_id
        );
      if (checkDuplicateWarehouseAndItem) {
        result = {
          message: 'warehouse_id and item_id combination already exist',
          status: false,
          data: null,
        };
        return result;
      }
    }
    const warehouseInventoryDetails = await warehouseInventoryDao.add(
      warehouse_id,
      product_id,
      quantity,
      item_id,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: warehouseInventoryDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in warehouseInventory service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing warehouseInventory
 * @param body
 * @returns
 */
const updateWarehouseInventory = async (body: updateWarehouseInventoryBody) => {
  try {
    const {
      warehouse_id,
      product_id,
      quantity,
      item_id,
      updated_by,
      warehouse_inventory_id,
    } = body;
    let result = null;

    const warehouseExist = await warehouseDao.getById(warehouse_id);
    if (!warehouseExist) {
      result = {
        message: 'warehouse_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    if (warehouse_id && item_id) {
      const checkDuplicateWarehouseAndItem =
        await warehouseInventoryDao.checkDuplicateWarehouseIdAndItemId(
          warehouse_id,
          item_id
        );
      if (checkDuplicateWarehouseAndItem) {
        result = {
          message: 'warehouse_id and item_id combination already exist',
          status: false,
          data: null,
        };
        return result;
      }
    }
    const warehouseInventoryExist = await warehouseInventoryDao.getById(
      warehouse_inventory_id
    );

    if (warehouseInventoryExist) {
      const warehouseInventoryDetails = await warehouseInventoryDao.edit(
        warehouse_id,
        product_id,
        quantity,
        item_id,
        updated_by,
        warehouse_inventory_id
      );
      result = {
        message: 'success',
        status: true,
        data: warehouseInventoryDetails,
      };
      return result;
    } else {
      result = {
        message: 'warehouse_inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in warehouseInventory service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get warehouseInventory By WarehouseInventoryId
 * @param warehouseInventoryId
 * @returns
 */
const getById = async (warehouseInventoryId: number) => {
  try {
    let result = null;
    const warehouseInventoryData = await warehouseInventoryDao.getById(
      warehouseInventoryId
    );
    if (warehouseInventoryData) {
      result = {
        message: 'success',
        status: true,
        data: warehouseInventoryData,
      };
      return result;
    } else {
      result = {
        message: 'warehouse_inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getById warehouseInventory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All warehouseInventory's
 * @returns
 */
const getAllWarehouseInventory = async () => {
  try {
    const result = await warehouseInventoryDao.getAll();
    const warehouseInventoryData = {
      message: 'success',
      status: true,
      data: result,
    };
    return warehouseInventoryData;
  } catch (error) {
    console.log(
      'Error occurred in  warehouseInventory service : getAllWarehouseInventory',
      error
    );
    throw error;
  }
};

/**
 * Method to delete warehouseInventory
 * @param warehouseInventoryId
 */
const deleteWarehouseInventory = async (warehouseInventoryId: number) => {
  try {
    const warehouseInventoryExist = await warehouseInventoryDao.getById(
      warehouseInventoryId
    );
    if (!warehouseInventoryExist) {
      const result = {
        message: 'warehouse_inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await warehouseInventoryDao.deleteWarehouseInventory(
      warehouseInventoryId
    );
    if (data) {
      const result = {
        message: 'warehouseInventory Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this warehouseInventory',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteWarehouseInventory warehouseInventory service : ',
      error
    );
    throw error;
  }
};

export {
  createWarehouseInventory,
  updateWarehouseInventory,
  getAllWarehouseInventory,
  getById,
  deleteWarehouseInventory,
};
