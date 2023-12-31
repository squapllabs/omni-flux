import prisma from '../utils/prisma';

const add = async (
  project_id: number,
  item_id: number,
  rate: number,
  available_quantity: number,
  total_cost: number,
  created_by: number,
  site_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.create({
      data: {
        project_id,
        item_id,
        rate,
        available_quantity,
        total_cost,
        site_id,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log('Error occurred in projectInventoryDao add', error);
    throw error;
  }
};

const edit = async (
  project_id: number,
  item_id: number,
  rate: number,
  available_quantity: number,
  total_cost: number,
  updated_by: number,
  site_id: number,
  project_inventory_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.update({
      where: {
        project_inventory_id: project_inventory_id,
      },
      data: {
        project_id,
        item_id,
        rate,
        available_quantity,
        total_cost,
        updated_by,
        site_id,
        updated_date: currentDate,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log('Error occurred in projectInventoryDao edit', error);
    throw error;
  }
};

const getById = async (projectInventoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.findFirst({
      where: {
        project_inventory_id: Number(projectInventoryId),
        is_delete: false,
      },
      include: {
        project_data: true,
        item_data: true,
        site_data: true,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log('Error occurred in projectInventory getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.findMany({
      where: {
        is_delete: false,
      },
      include: {
        project_data: true,
        item_data: true,
        site_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return projectInventory;
  } catch (error) {
    console.log('Error occurred in projectInventory getAll dao', error);
    throw error;
  }
};

const deleteProjectInventory = async (
  projectInventoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.update({
      where: {
        project_inventory_id: Number(projectInventoryId),
      },
      data: {
        is_delete: true,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log(
      'Error occurred in projectInventory deleteProjectInventory dao',
      error
    );
    throw error;
  }
};

const searchProjectInventory = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterProjectInventory;
    const projectInventory = await transaction.project_inventory.findMany({
      where: filter,
      include: {
        project_data: true,
        item_data: true,
        site_data: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const projectInventoryCount = await transaction.project_inventory.count({
      where: filter,
    });
    const projectInventoryData = {
      count: projectInventoryCount,
      data: projectInventory,
    };
    return projectInventoryData;
  } catch (error) {
    console.log(
      'Error occurred in projectInventory dao : searchProjectInventory',
      error
    );
    throw error;
  }
};

const getByProjectIdAndItemId = async (
  project_id: number,
  item_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.findFirst({
      where: {
        project_id: Number(project_id),
        item_id: Number(item_id),
        is_delete: false,
      },
      include: {
        project_data: true,
        item_data: true,
        site_data: true,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log(
      'Error occurred in projectInventory getByProjectIdAndItemId dao',
      error
    );
    throw error;
  }
};

const updateQuantityByProjectInventoryId = async (
  available_quantity: number,
  updated_by: number,
  total_cost: number,
  project_inventory_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.update({
      where: {
        project_inventory_id: project_inventory_id,
      },
      data: {
        available_quantity,
        updated_by,
        total_cost,
        updated_date: currentDate,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log(
      'Error occurred in projectInventoryDao updateQuantityByProjectInventoryId',
      error
    );
    throw error;
  }
};

const getByProjectId = async (project_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.findMany({
      where: {
        project_id: Number(project_id),
        is_delete: false,
      },
      include: {
        project_data: true,
        item_data: { include: { uom: { select: { name: true } } } },
        site_data: true,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log('Error occurred in projectInventory getByProjectId dao', error);
    throw error;
  }
};

const getByProjectIdAndSiteId = async (
  project_id: number,
  site_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.findMany({
      where: {
        project_id: Number(project_id),
        site_id: Number(site_id),
        is_delete: false,
      },
      include: {
        project_data: true,
        item_data: { include: { uom: { select: { name: true } } } },
        site_data: true,
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return projectInventory;
  } catch (error) {
    console.log(
      'Error occurred in projectInventory getByProjectIdAndSiteId dao',
      error
    );
    throw error;
  }
};

const getByProjectSiteItem = async (
  project_id: number,
  site_id: number,
  item_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const projectInventory = await transaction.project_inventory.findFirst({
      where: {
        project_id: Number(project_id),
        site_id: Number(site_id),
        item_id: Number(item_id),
        is_delete: false,
      },
      include: {
        project_data: true,
        item_data: true,
        site_data: true,
      },
    });
    return projectInventory;
  } catch (error) {
    console.log(
      'Error occurred in projectInventory getByProjectSiteItem dao',
      error
    );
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteProjectInventory,
  searchProjectInventory,
  getByProjectIdAndItemId,
  updateQuantityByProjectInventoryId,
  getByProjectId,
  getByProjectIdAndSiteId,
  getByProjectSiteItem,
};
