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
      code,
    } = body;
    let result = null;

    const siteContractorDetails = await siteContractorDao.add(
      name,
      type,
      mobile_number,
      contact_number,
      address,
      description,
      created_by,
      code
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
      code,
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
        code,
        site_contractor_id
      );
      result = {
        message: 'success',
        status: true,
        data: siteContractorDetails,
      };
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

/**
 * Method to search Site Contractor - Pagination API
 * @returns
 */
const searchSiteContractor = async (body) => {
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
    const type = body.type;
    const filterObj = {
      filterSiteContractor: {
        AND: type ? [{ type: type }] : [],
        /* OR: [
          {
            name: { contains: global_search, mode: 'insensitive' },
          },
          {
            description: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
          {
            address: {
              path: ['street'],
              string_contains: global_search,
            },
          }, */
        /*  {
            address: {
              street: { contains: global_search, mode: 'insensitive' },
              city: { contains: global_search, mode: 'insensitive' },
              state: { contains: global_search, mode: 'insensitive' },
              pin_code: { contains: global_search, mode: 'insensitive' },
              country: { contains: global_search, mode: 'insensitive' },
            },
          }, */
        /* ], */
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await siteContractorDao.searchSiteContractor(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj,
      global_search
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempSiteContractorData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempSiteContractorData;
  } catch (error) {
    console.log(
      'Error occurred in searchSiteContractor Site Contractor service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to get SiteContractor By SiteContractor Code
 * @param code
 * @returns
 */
const getByCode = async (code: string) => {
  try {
    let result = null;
    const siteData = await siteContractorDao.getByCode(code);
    if (siteData) {
      result = {
        message: 'This code is already exist',
        status: true,
        is_exist: true,
        data: siteData,
      };
      return result;
    } else {
      result = {
        message: 'This code is not exist',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in getByCode site contractor service : ',
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
  searchSiteContractor,
  getByCode,
};
