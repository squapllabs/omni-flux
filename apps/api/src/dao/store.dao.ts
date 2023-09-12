import prisma from '../utils/prisma';

const add = async (
  store_name: string,
  store_manager_id: number,
  address: JSON,
  contact_email: string,
  contact_phone: string,
  project_id: number,
  site_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const store = await transaction.store.create({
      data: {
        store_name,
        store_manager_id,
        address,
        contact_email,
        contact_phone,
        project_id,
        site_id,
        created_by,
        is_delete: is_delete,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });
    return store;
  } catch (error) {
    console.log('Error occurred in storeDao add', error);
    throw error;
  }
};

const edit = async (
  store_name: string,
  store_manager_id: number,
  address: JSON,
  contact_email: string,
  contact_phone: string,
  project_id: number,
  site_id: number,
  updated_by: number,
  store_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const store = await transaction.store.update({
      where: {
        store_id: store_id,
      },
      data: {
        store_name,
        store_manager_id,
        address,
        contact_email,
        contact_phone,
        project_id,
        site_id,
        updated_by,
        updated_date: currentDate,
      },
    });
    return store;
  } catch (error) {
    console.log('Error occurred in storeDao edit', error);
    throw error;
  }
};

const getById = async (storeId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const store = await transaction.store.findFirst({
      where: {
        store_id: Number(storeId),
        is_delete: false,
      },
      include: {
        store_manager_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        site_data: true,
      },
    });
    return store;
  } catch (error) {
    console.log('Error occurred in store getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const store = await transaction.store.findMany({
      where: {
        is_delete: false,
      },
      include: {
        store_manager_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        site_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return store;
  } catch (error) {
    console.log('Error occurred in store getAll dao', error);
    throw error;
  }
};

const deleteStore = async (storeId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const store = await transaction.store.update({
      where: {
        store_id: Number(storeId),
      },
      data: { is_delete: true },
    });
    return store;
  } catch (error) {
    console.log('Error occurred in store deleteStore dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteStore,
};
