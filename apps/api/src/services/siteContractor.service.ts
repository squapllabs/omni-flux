import siteContractorDao from '../dao/siteContractor.dao';
import {
  createSiteContractorBody,
  updateSiteContractorBody,
} from '../interfaces/siteContractor.Interface';

/**
 * Method to Create a New SiteContractor
 * @param body
 * @returns
 */
const createSiteContractor = async (body: createSiteContractorBody) => {
  try {
    const {
      name,
      type,
      mobile_number,
      contact_number,
      address,
      description,
      created_by,
    } = body;
    let result = null;

    const siteContractorDetails = await siteContractorDao.add(
      name,
      type,
      mobile_number,
      contact_number,
      address,
      description,
      created_by
    );
    result = { message: 'success', status: true, data: siteContractorDetails };

    return result;
  } catch (error) {
    console.log('Error occurred in site contractor service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing SiteContractor
 * @param body
 * @returns
 */

const updateSiteContractor = async (body: updateSiteContractorBody) => {
  try {
    const {
      name,
      type,
      mobile_number,
      contact_number,
      address,
      description,
      updated_by,
      site_contractor_id,
    } = body;
    let result = null;

    const siteContractorExist = await siteContractorDao.getById(
      site_contractor_id
    );
    if (siteContractorExist) {
      const siteContractorDetails = await siteContractorDao.edit(
        name,
        type,
        mobile_number,
        contact_number,
        address,
        description,
        updated_by,
        site_contractor_id
      );
      result = {
        message: 'success',
        status: true,
        data: siteContractorDetails,
      };
      return result;
    } else {
      result = { success: false, message: 'site_contractor_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in site contractor service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get SiteContractor By SiteContractorId
 * @param siteContractorId
 * @returns
 */
const getById = async (siteContractorId: number) => {
  try {
    let result = null;
    const siteData = await siteContractorDao.getById(siteContractorId);
    if (siteData) {
      result = { message: 'success', status: true, data: siteData };
      return result;
    } else {
      result = {
        message: 'site_contractor_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById site contractor service : ', error);
    throw error;
  }
};

/**
 * Method to Get All SiteContractor's
 * @returns
 */
const getAllSiteContractors = async () => {
  try {
    const result = await siteContractorDao.getAll();
    const siteData = { massage: 'success', status: true, data: result };
    return siteData;
  } catch (error) {
    console.log(
      'Error occurred in site contractor service getAllSiteContractors: ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete site Contractor
 * @param siteContractorId
 */
const deleteSiteContractor = async (siteContractorId: number) => {
  try {
    const siteContractorExist = await siteContractorDao.getById(
      siteContractorId
    );
    if (!siteContractorExist) {
      const result = {
        message: 'site_contractor_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await siteContractorDao.deleteSiteContractor(siteContractorId);
    if (data) {
      const result = {
        message: 'SiteContractor Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this site',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteSiteContractor site contractor service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All Site's
 * @returns
 */
const getAllSites = async () => {
  try {
    const result = await siteContractorDao.getAllSites();
    const siteData = { massage: 'success', status: true, data: result };
    return siteData;
  } catch (error) {
    console.log(
      'Error occurred in site contractor service getAllSites: ',
      error
    );
    throw error;
  }
};

/**
 * Method to Get All Contractors's
 * @returns
 */
const getAllContractors = async () => {
  try {
    const result = await siteContractorDao.getAllContractors();
    const siteData = { massage: 'success', status: true, data: result };
    return siteData;
  } catch (error) {
    console.log(
      'Error occurred in site contractor service getAllSites: ',
      error
    );
    throw error;
  }
};

export {
  createSiteContractor,
  updateSiteContractor,
  getAllSiteContractors,
  getById,
  deleteSiteContractor,
  getAllSites,
  getAllContractors,
};
