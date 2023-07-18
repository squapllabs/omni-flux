import warehouseDao from '../dao/warehouse.dao';
import {
  createWarehouseBody,
  updateWarehouseBody,
} from '../interfaces/warehouse.interface';

/**
 * Method to Create a New Warehouse
 * @param body
 * @returns
 */
const createWarehouse = async (body: createWarehouseBody) => {
  try {
    const { warehouse_name, location, created_by = null } = body;
    const warehouseDetails = await warehouseDao.add(
      warehouse_name,
      location,
      created_by
    );
    const result = { success: true, data: warehouseDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in warehouse service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Warehouse
 * @param body
 * @returns
 */
const updateWarehouse = async (body: updateWarehouseBody) => {
  try {
    const { warehouse_name, location, updated_by, warehouse_id } = body;
    let result = null;
    const warehouseExist = await warehouseDao.getById(warehouse_id);
    if (warehouseExist) {
      const warehouseDetails = await warehouseDao.edit(
        warehouse_name,
        location,
        updated_by,
        warehouse_id
      );
      result = { success: true, data: warehouseDetails };
      return result;
    } else {
      result = { success: false, message: 'warehouse_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in warehouse service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Warehouse By WarehouseId
 * @param warehouseId
 * @returns
 */
const getById = async (warehouseId: number) => {
  try {
    let result = null;
    const warehouseData = await warehouseDao.getById(warehouseId);
    if (warehouseData) {
      result = { success: true, data: warehouseData };
      return result;
    } else {
      result = { success: false, message: 'warehouse_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById warehouse service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Warehouse's
 * @returns
 */
const getAllWarehouse = async () => {
  try {
    const result = await warehouseDao.getAll();
    const warehouseData = { success: true, data: result };
    return warehouseData;
  } catch (error) {
    console.log(
      'Error occurred in getAllWarehouse warehouse service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete warehouse
 * @param warehouseId
 */
const deleteWarehouse = async (warehouseId: number) => {
  try {
    const warehouseExist = await warehouseDao.getById(warehouseId);
    if (!warehouseExist) {
      const result = { success: false, message: 'warehouse_id Not Exist' };
      return result;
    }
    const data = await warehouseDao.deleteWarehouse(warehouseId);
    if (data) {
      const result = {
        success: true,
        message: 'Warehouse Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this warehouse',
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteWarehouse warehouse service : ',
      error
    );
    throw error;
  }
};

export {
  createWarehouse,
  updateWarehouse,
  getAllWarehouse,
  getById,
  deleteWarehouse,
};
