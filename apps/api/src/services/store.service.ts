import projectDao from '../dao/project.dao';
import siteContractorDao from '../dao/siteContractor.dao';
import storeDao from '../dao/store.dao';
import userDao from '../dao/user.dao';
import { storeBody } from '../interfaces/store.interface';

/**
 * Method to Create a New Store
 * @param body
 * @returns
 */
const createStore = async (body: storeBody) => {
  try {
    const {
      store_name,
      store_manager_id,
      address,
      contact_email,
      contact_phone,
      project_id,
      site_id,
      created_by,
    } = body;

    if (store_manager_id) {
      const storeManagerExist = await userDao.getById(store_manager_id);
      if (!storeManagerExist) {
        return {
          message: 'store_manager_id does not exist',
          status: false,
          data: null,
        };
      }
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

    if (site_id) {
      const siteExist = await siteContractorDao.getBySiteId(site_id);
      if (!siteExist) {
        return {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const storeDetails = await storeDao.add(
      store_name,
      store_manager_id,
      address,
      contact_email,
      contact_phone,
      project_id,
      site_id,
      created_by
    );
    const result = { message: 'success', status: true, data: storeDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in store service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Store
 * @param body
 * @returns
 */
const updateStore = async (body: storeBody) => {
  try {
    const {
      store_name,
      store_manager_id,
      address,
      contact_email,
      contact_phone,
      project_id,
      site_id,
      updated_by,
      store_id,
    } = body;
    let result = null;
    const storeExist = await storeDao.getById(store_id);
    if (!storeExist) {
      result = {
        message: 'store_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }

    if (store_manager_id) {
      const storeManagerExist = await userDao.getById(store_manager_id);
      if (!storeManagerExist) {
        return {
          message: 'store_manager_id does not exist',
          status: false,
          data: null,
        };
      }
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

    if (site_id) {
      const siteExist = await siteContractorDao.getBySiteId(site_id);
      if (!siteExist) {
        return {
          message: 'site_id does not exist',
          status: false,
          data: null,
        };
      }
    }

    const storeDetails = await storeDao.edit(
      store_name,
      store_manager_id,
      address,
      contact_email,
      contact_phone,
      project_id,
      site_id,
      updated_by,
      store_id
    );
    result = { message: 'success', status: true, data: storeDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in store service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Store By StoreId
 * @param storeId
 * @returns
 */
const getById = async (storeId: number) => {
  try {
    let result = null;
    const storeData = await storeDao.getById(storeId);
    if (storeData) {
      result = { message: 'success', status: true, data: storeData };
      return result;
    } else {
      result = {
        message: 'store_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById store service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Store's
 * @returns
 */
const getAllStore = async () => {
  try {
    const result = await storeDao.getAll();
    const storeData = { message: 'success', status: true, data: result };
    return storeData;
  } catch (error) {
    console.log('Error occurred in getAllStore store service : ', error);
    throw error;
  }
};

/**
 * Method to delete store
 * @param storeId
 */
const deleteStore = async (storeId: number) => {
  try {
    const storeExist = await storeDao.getById(storeId);
    if (!storeExist) {
      const result = {
        message: 'store_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await storeDao.deleteStore(storeId);
    if (data) {
      const result = {
        message: 'Store Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this store',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteStore store service : ', error);
    throw error;
  }
};

export { createStore, updateStore, getAllStore, getById, deleteStore };
