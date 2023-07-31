import leadProductDao from '../dao/leadProduct.dao';
import {
  createLeadProductBody,
  updateLeadProductBody,
} from '../interfaces/leadProduct.Interface';
import leadEnquiryDao from '../dao/leadEnquiry.dao';

/**
 * Method to Create a New LeadProduct
 * @param body
 * @returns
 */
const createLeadProduct = async (body: createLeadProductBody) => {
  try {
    const {
      lead_enquiry_id,
      source_name,
      probability,
      our_remarks,
      client_remark,
      approx_value,
      sales_person_name,
      created_by,
    } = body;
    let result = null;
    if (lead_enquiry_id) {
      const leadEnquiryExist = await leadEnquiryDao.getById(
        Number(lead_enquiry_id)
      );
      if (!leadEnquiryExist) {
        result = {
          message: 'lead_enquiry_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const leadProductDetails = await leadProductDao.add(
      lead_enquiry_id,
      source_name,
      probability,
      our_remarks,
      client_remark,
      approx_value,
      sales_person_name,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: leadProductDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in leadProduct service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing LeadProduct
 * @param body
 * @returns
 */
const updateLeadProduct = async (body: updateLeadProductBody) => {
  try {
    const {
      lead_enquiry_id,
      source_name,
      probability,
      our_remarks,
      client_remark,
      approx_value,
      sales_person_name,
      updated_by,
      lead_product_id,
    } = body;
    let result = null;
    const leadProductExist = await leadProductDao.getById(lead_product_id);

    if (lead_enquiry_id) {
      const leadEnquiryExist = await leadEnquiryDao.getById(
        Number(lead_enquiry_id)
      );
      if (!leadEnquiryExist) {
        result = {
          message: 'lead_enquiry_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (leadProductExist) {
      const leadProductDetails = await leadProductDao.edit(
        lead_enquiry_id,
        source_name,
        probability,
        our_remarks,
        client_remark,
        approx_value,
        sales_person_name,
        updated_by,
        lead_product_id
      );
      result = {
        message: 'success',
        status: true,
        data: leadProductDetails,
      };
      return result;
    } else {
      result = {
        message: 'lead_product_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in leadProduct service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get LeadProduct By LeadProductId
 * @param leadProductId
 * @returns
 */
const getById = async (leadProductId: number) => {
  try {
    let result = null;
    const leadProductData = await leadProductDao.getById(leadProductId);
    if (leadProductData) {
      result = { message: 'success', status: true, data: leadProductData };
      return result;
    } else {
      result = {
        message: 'lead_product_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById leadProduct service : ', error);
    throw error;
  }
};

/**
 * Method to Get All LeadProduct's
 * @returns
 */
const getAllLeadProduct = async () => {
  try {
    const result = await leadProductDao.getAll();
    const leadProductData = { message: 'success', status: true, data: result };
    return leadProductData;
  } catch (error) {
    console.log(
      'Error occurred in getAllLeadProduct leadProduct service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete leadProduct
 * @param leadProductId
 */
const deleteLeadProduct = async (leadProductId: number) => {
  try {
    const leadProductExist = await leadProductDao.getById(leadProductId);
    if (!leadProductExist) {
      const result = {
        status: false,
        message: 'lead_product_id does not exist',
        data: null,
      };
      return result;
    }

    const data = await leadProductDao.deleteLeadProduct(leadProductId);
    if (data) {
      const result = {
        status: true,
        message: 'LeadProduct Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this leadProduct',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteLeadProduct leadProduct service : ',
      error
    );
    throw error;
  }
};

export {
  createLeadProduct,
  updateLeadProduct,
  getAllLeadProduct,
  getById,
  deleteLeadProduct,
};
