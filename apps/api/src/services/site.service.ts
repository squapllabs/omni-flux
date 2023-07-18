import siteDao from '../dao/site.dao';
import userDao from '../dao/user.dao';
import { createSiteBody, updateSiteBody } from '../interfaces/site.Interface';

/**
 * Method to Create a New Site
 * @param body
 * @returns
 */
const createSite = async (body: createSiteBody) => {
  try {
    const { site_name, location, user_id, created_by } = body;
    let result = null;
    const userExist = await userDao.getById(user_id);
    if (!userExist) {
      result = { success: false, message: 'user_id does not exist' };
      return result;
    }
    const siteDetails = await siteDao.add(
      site_name,
      location,
      user_id,
      created_by
    );
    result = { success: true, data: siteDetails };
    console.log('result', result);

    return result;
  } catch (error) {
    console.log('Error occurred in site service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Site
 * @param body
 * @returns
 */

const updateSite = async (body: updateSiteBody) => {
  try {
    const { site_name, location, user_id, updated_by, site_id } = body;
    let result = null;
    const userExist = await userDao.getById(user_id);
    if (!userExist) {
      result = { success: false, message: 'user_id does not exist' };
      return result;
    }
    const siteExist = await siteDao.getById(site_id);
    if (siteExist) {
      const siteDetails = await siteDao.edit(
        site_name,
        location,
        user_id,
        updated_by,
        site_id
      );
      result = { success: true, data: siteDetails };
      return result;
    } else {
      result = { success: false, message: 'site_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in site service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Site By SiteId
 * @param siteId
 * @returns
 */
const getById = async (siteId: number) => {
  try {
    let result = null;
    const siteData = await siteDao.getById(siteId);
    if (siteData) {
      result = { success: true, data: siteData };
      return result;
    } else {
      result = { success: false, message: 'site_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById site service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Site's
 * @returns
 */
const getAllSites = async () => {
  try {
    const result = await siteDao.getAll();
    const siteData = { success: true, data: result };
    return siteData;
  } catch (error) {
    console.log('Error occurred in site service getAllSites: ', error);
    throw error;
  }
};

/**
 * Method to delete site
 * @param siteId
 */
const deleteSite = async (siteId: number) => {
  try {
    const siteExist = await siteDao.getById(siteId);
    if (!siteExist) {
      const result = { success: false, message: 'site_id does not exist' };
      return result;
    }
    const data = await siteDao.deleteSite(siteId);
    if (data) {
      const result = {
        success: true,
        message: 'Site Data Deleted Successfully',
      };
      return result;
    } else {
      const result = { success: false, message: 'Failed to delete this site' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteSite site service : ', error);
    throw error;
  }
};

export { createSite, updateSite, getAllSites, getById, deleteSite };
