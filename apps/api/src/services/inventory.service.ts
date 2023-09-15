import inventoryDao from '../dao/inventory.dao';
import { inventoryBody } from '../interfaces/inventory.interface';

/**
 * Method to Create a New Inventory
 * @param body
 * @returns
 */
const createInventory = async (body: inventoryBody) => {
  try {
    const {
      item_name,
      item_category,
      rate,
      available_quantity,
      store_id,
      project_id,
      created_by,
    } = body;
    const inventoryDetails = await inventoryDao.add(
      item_name,
      item_category,
      rate,
      available_quantity,
      store_id,
      project_id,
      created_by
    );
    const result = { message: 'success', status: true, data: inventoryDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in inventory service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Inventory
 * @param body
 * @returns
 */

const updateInventory = async (body: inventoryBody) => {
  try {
    const {
      item_name,
      item_category,
      rate,
      available_quantity,
      store_id,
      project_id,
      updated_by,
      inventory_id,
    } = body;
    let result = null;
    const inventoryExist = await inventoryDao.getById(inventory_id);
    if (!inventoryExist) {
      result = {
        message: 'inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    } else {
      const inventoryDetails = await inventoryDao.edit(
        item_name,
        item_category,
        rate,
        available_quantity,
        store_id,
        project_id,
        updated_by,
        inventory_id
      );
      result = { message: 'success', status: true, data: inventoryDetails };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in inventory service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Inventory By InventoryId
 * @param inventoryId
 * @returns
 */
const getById = async (inventoryId: number) => {
  try {
    let result = null;
    const inventoryData = await inventoryDao.getById(inventoryId);
    if (inventoryData) {
      result = { message: 'success', status: true, data: inventoryData };
      return result;
    } else {
      result = {
        message: 'inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById inventory service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Inventorys
 * @returns
 */
const getAllInventorys = async () => {
  try {
    const result = await inventoryDao.getAll();
    const inventoryData = { message: 'success', status: true, data: result };
    return inventoryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllInventorys inventory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete inventory
 * @param inventoryId
 */
const deleteInventory = async (inventoryId: number) => {
  try {
    const inventoryExist = await inventoryDao.getById(inventoryId);

    if (!inventoryExist) {
      const result = {
        message: 'inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await inventoryDao.deleteInventory(inventoryId);
    if (data) {
      const result = {
        message: 'Inventory Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this inventory',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteInventory inventory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search Inventory - Pagination API
 * @returns
 */
const searchInventory = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;
    const store_id = body.store_id;
    const filterObj: any = {};

    if (status) {
      filterObj.filterInventory = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (store_id) {
      filterObj.filterInventory = filterObj.filterInventory || {};
      filterObj.filterInventory.AND = filterObj.filterInventory.AND || [];

      filterObj.filterInventory.AND.push({
        store_id: {
          equals: store_id,
        },
      });
    }

    if (global_search) {
      filterObj.filterInventory = filterObj.filterInventory || {};
      filterObj.filterInventory.OR = filterObj.filterInventory.OR || [];

      filterObj.filterInventory.OR.push(
        {
          item_name: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          item_category: {
            contains: global_search,
            mode: 'insensitive',
          },
        },
        {
          project_data: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          store_data: {
            store_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          store_data: {
            store_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          store_data: {
            store_manager_data: {
              first_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          store_data: {
            store_manager_data: {
              last_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        }
      );
    }

    const result = await inventoryDao.searchInventory(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempInventoryData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempInventoryData;
  } catch (error) {
    console.log(
      'Error occurred in searchInventory Inventory service : ',
      error
    );
    throw error;
  }
};

export {
  createInventory,
  updateInventory,
  getAllInventorys,
  getById,
  deleteInventory,
  searchInventory,
};
