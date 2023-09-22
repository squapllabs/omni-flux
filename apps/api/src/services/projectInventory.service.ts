import itemDao from '../dao/item.dao';
import projectDao from '../dao/project.dao';
import projectInventoryDao from '../dao/projectInventory.dao';
import { projectInventoryBody } from '../interfaces/projectInventory.interface';

/**
 * Method to Create a New ProjectInventory
 * @param body
 * @returns
 */
const createProjectInventory = async (body: projectInventoryBody) => {
  try {
    const {
      project_id,
      item_id,
      rate,
      available_quantity,
      total_cost,
      created_by,
    } = body;

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (item_id) {
      const itemExist = await itemDao.getById(item_id);
      if (!itemExist) {
        return {
          message: 'item_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const projectInventoryDetails = await projectInventoryDao.add(
      project_id,
      item_id,
      rate,
      available_quantity,
      total_cost,
      created_by
    );
    const result = {
      message: 'success',
      status: true,
      data: projectInventoryDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in projectInventory service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing ProjectInventory
 * @param body
 * @returns
 */

const updateProjectInventory = async (body: projectInventoryBody) => {
  try {
    const {
      project_id,
      item_id,
      rate,
      available_quantity,
      total_cost,
      updated_by,
      project_inventory_id,
    } = body;
    let result = null;
    const projectInventoryExist = await projectInventoryDao.getById(
      project_inventory_id
    );
    if (!projectInventoryExist) {
      result = {
        message: 'project_inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (project_id) {
      const projectExist = await projectDao.getById(project_id);
      if (!projectExist) {
        return {
          message: 'project_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    if (item_id) {
      const itemExist = await itemDao.getById(item_id);
      if (!itemExist) {
        return {
          message: 'item_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const projectInventoryDetails = await projectInventoryDao.edit(
      project_id,
      item_id,
      rate,
      available_quantity,
      total_cost,
      updated_by,
      project_inventory_id
    );
    result = {
      message: 'success',
      status: true,
      data: projectInventoryDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in projectInventory service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get ProjectInventory By ProjectInventoryId
 * @param projectInventoryId
 * @returns
 */
const getById = async (projectInventoryId: number) => {
  try {
    let result = null;
    const projectInventoryData = await projectInventoryDao.getById(
      projectInventoryId
    );
    if (projectInventoryData) {
      result = { message: 'success', status: true, data: projectInventoryData };
      return result;
    } else {
      result = {
        message: 'project_inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById projectInventory service : ', error);
    throw error;
  }
};

/**
 * Method to Get All ProjectInventorys
 * @returns
 */
const getAllProjectInventorys = async () => {
  try {
    const result = await projectInventoryDao.getAll();
    const projectInventoryData = {
      message: 'success',
      status: true,
      data: result,
    };
    return projectInventoryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllProjectInventorys projectInventory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete projectInventory
 * @param projectInventoryId
 */
const deleteProjectInventory = async (projectInventoryId: number) => {
  try {
    const projectInventoryExist = await projectInventoryDao.getById(
      projectInventoryId
    );

    if (!projectInventoryExist) {
      const result = {
        message: 'project_inventory_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    const data = await projectInventoryDao.deleteProjectInventory(
      projectInventoryId
    );
    if (data) {
      const result = {
        message: 'ProjectInventory Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this projectInventory',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteProjectInventory projectInventory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to search ProjectInventory - Pagination API
 * @returns
 */
const searchProjectInventory = async (body) => {
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
    const project_id = body.project_id;

    const filterObj: any = {};

    if (status) {
      filterObj.filterProjectInventory = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (project_id) {
      filterObj.filterProjectInventory = filterObj.filterProjectInventory || {};
      filterObj.filterProjectInventory.AND =
        filterObj.filterProjectInventory.AND || [];
      filterObj.filterProjectInventory.AND.push({
        project_id: project_id,
      });
    }

    if (global_search) {
      filterObj.filterProjectInventory = filterObj.filterProjectInventory || {};
      filterObj.filterProjectInventory.OR =
        filterObj.filterProjectInventory.OR || [];

      filterObj.filterProjectInventory.OR.push(
        {
          project_data: {
            project_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },
        {
          item_data: {
            item_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        }
      );
    }

    const result = await projectInventoryDao.searchProjectInventory(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempProjectInventoryData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempProjectInventoryData;
  } catch (error) {
    console.log(
      'Error occurred in searchProjectInventory ProjectInventory service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get ProjectInventory By Project Id
 * @param project_id
 * @returns
 */
const getByProjectId = async (project_id: number) => {
  try {
    let result = null;

    const projectExist = await projectDao.getById(project_id);
    if (!projectExist) {
      return {
        message: 'project_id does not exist',
        status: false,
        data: null,
      };
    }

    const projectInventoryData = await projectInventoryDao.getByProjectId(
      project_id
    );
    if (projectInventoryData) {
      result = { message: 'success', status: true, data: projectInventoryData };
      return result;
    } else {
      result = {
        message: 'No data found for this project_id',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByProjectId projectInventory service : ',
      error
    );
    throw error;
  }
};

export {
  createProjectInventory,
  updateProjectInventory,
  getAllProjectInventorys,
  getById,
  deleteProjectInventory,
  searchProjectInventory,
  getByProjectId,
};
