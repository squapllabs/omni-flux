import vendorDao from '../dao/vendor.dao';
import { vendorBody } from '../interfaces/vendor.interface';

/**
 * Method to Create a New Vendor
 * @param body
 * @returns
 */
const createVendor = async (body: vendorBody) => {
  try {
    const {
      vendor_name,
      contact_person,
      contact_email,
      contact_phone_no,
      address,
      tax_id,
      payment_terms,
      preferred_payment_method_id,
      bank_account_details,
      currency,
      vendor_category_id,
      lead_time,
      minimum_order_quantity,
      notes,
      created_by,
    } = body;
    const vendorDetails = await vendorDao.add(
      vendor_name,
      contact_person,
      contact_email,
      contact_phone_no,
      address,
      tax_id,
      payment_terms,
      preferred_payment_method_id,
      bank_account_details,
      currency,
      vendor_category_id,
      lead_time,
      minimum_order_quantity,
      notes,
      created_by
    );
    const result = { message: 'success', status: true, data: vendorDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in vendor service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing Vendor
 * @param body
 * @returns
 */
const updateVendor = async (body: vendorBody) => {
  try {
    const {
      vendor_name,
      contact_person,
      contact_email,
      contact_phone_no,
      address,
      tax_id,
      payment_terms,
      preferred_payment_method_id,
      bank_account_details,
      currency,
      vendor_category_id,
      lead_time,
      minimum_order_quantity,
      notes,
      updated_by,
      vendor_id,
    } = body;
    let result = null;
    const vendorExist = await vendorDao.getById(vendor_id);
    if (vendorExist) {
      const vendorDetails = await vendorDao.edit(
        vendor_name,
        contact_person,
        contact_email,
        contact_phone_no,
        address,
        tax_id,
        payment_terms,
        preferred_payment_method_id,
        bank_account_details,
        currency,
        vendor_category_id,
        lead_time,
        minimum_order_quantity,
        notes,
        updated_by,
        vendor_id
      );
      result = { message: 'success', status: true, data: vendorDetails };
      return result;
    } else {
      result = {
        message: 'vendor_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in vendor service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get Vendor By VendorId
 * @param vendorId
 * @returns
 */
const getById = async (vendorId: number) => {
  try {
    let result = null;
    const vendorData = await vendorDao.getById(vendorId);
    if (vendorData) {
      result = { message: 'success', status: true, data: vendorData };
      return result;
    } else {
      result = {
        message: 'vendor_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById vendor service : ', error);
    throw error;
  }
};

/**
 * Method to Get All Vendor's
 * @returns
 */
const getAllVendor = async () => {
  try {
    const result = await vendorDao.getAll();
    const vendorData = { message: 'success', status: true, data: result };
    return vendorData;
  } catch (error) {
    console.log('Error occurred in getAllVendor vendor service : ', error);
    throw error;
  }
};

/**
 * Method to delete vendor
 * @param vendorId
 */
const deleteVendor = async (vendorId: number) => {
  try {
    const vendorExist = await vendorDao.getById(vendorId);
    if (!vendorExist) {
      const result = {
        message: 'vendor_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await vendorDao.deleteVendor(vendorId);
    if (data) {
      const result = {
        success: true,
        message: 'Vendor Data Deleted Successfully',
      };
      return result;
    } else {
      const result = {
        success: false,
        message: 'Failed to delete this vendor',
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteVendor vendor service : ', error);
    throw error;
  }
};

/**
 * Method to search Vendor - Pagination API
 * @returns
 */
const searchVendor = async (body) => {
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

    const result = await vendorDao.searchVendor(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      global_search,
      status,
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempVendorData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempVendorData;
  } catch (error) {
    console.log('Error occurred in searchVendor Vendor service : ', error);
    throw error;
  }
};

/**
 * Method to get Vendor By Email Id
 * @param contact_email
 * @returns
 */
const getByEmailId = async (contact_email: string) => {
  try {
    let result = null;
    const vendorData = await vendorDao.getByEmailId(contact_email);
    if (vendorData) {
      result = {
        message: 'contact_email is already exist',
        status: true,
        is_exist: true,
        data: vendorData,
      };
      return result;
    } else {
      result = {
        message: 'contact_email does not exist',
        status: false,
        is_exist: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByEmailId vendor service : ', error);
    throw error;
  }
};

export {
  createVendor,
  updateVendor,
  getAllVendor,
  getById,
  deleteVendor,
  searchVendor,
  getByEmailId,
};
