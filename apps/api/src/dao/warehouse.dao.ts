import prisma from '../utils/prisma';

const add = async (
  warehouse_name: string,
  location: string,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouse = await transaction.warehouse.create({
      data: {
        warehouse_name,
        location,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return warehouse;
  } catch (error) {
    console.log('Error occurred in warehouseDao add', error);
    throw error;
  }
};

const edit = async (
  warehouse_name: string,
  location: string,
  updated_by: bigint,
  warehouse_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouse = await transaction.warehouse.update({
      where: {
        warehouse_id: warehouse_id,
      },
      data: {
        warehouse_name,
        location,
        updated_by,
        updated_date: currentDate,
      },
    });
    return warehouse;
  } catch (error) {
    console.log('Error occurred in warehouseDao edit', error);
    throw error;
  }
};

const getById = async (warehouseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouse = await transaction.warehouse.findUnique({
      where: {
        warehouse_id: Number(warehouseId),
      },
    });
    return warehouse;
  } catch (error) {
    console.log('Error occurred in warehouse getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouse = await transaction.warehouse.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return warehouse;
  } catch (error) {
    console.log('Error occurred in warehouse getAll dao', error);
    throw error;
  }
};

const deleteWarehouse = async (warehouseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const warehouse = await transaction.warehouse.delete({
      where: {
        warehouse_id: Number(warehouseId),
      },
    });
    return warehouse;
  } catch (error) {
    console.log('Error occurred in warehouse deleteWarehouse dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteWarehouse,
};
