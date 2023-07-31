import leadTenderDao from '../dao/leadTender.dao';
import {
  createLeadTenderBody,
  updateLeadTenderBody,
} from '../interfaces/leadTender.Interface';
import leadEnquiryDao from '../dao/leadEnquiry.dao';

/**
 * Method to Create a New LeadTender
 * @param body
 * @returns
 */
const createLeadTender = async (body: createLeadTenderBody) => {
  try {
    const {
      lead_enquiry_id,
      approx_value,
      source_name,
      sales_person_name,
      tender_reg_no,
      tender_identification_no,
      tender_name,
      tender_issue_date,
      tender_due_date,
      tender_type,
      estimated_value,
      industry_sector,
      created_by,
    } = body;

    let result = null;

    if (lead_enquiry_id) {
      const leadEnquiryExist = await leadEnquiryDao.getById(lead_enquiry_id);
      if (!leadEnquiryExist) {
        result = {
          message: 'lead_enquiry_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    const leadTenderDetails = await leadTenderDao.add(
      lead_enquiry_id,
      approx_value,
      source_name,
      sales_person_name,
      tender_reg_no,
      tender_identification_no,
      tender_name,
      tender_issue_date,
      tender_due_date,
      tender_type,
      estimated_value,
      industry_sector,
      created_by
    );
    result = {
      message: 'success',
      status: true,
      data: leadTenderDetails,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in leadTender service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing LeadTender
 * @param body
 * @returns
 */
const updateLeadTender = async (body: updateLeadTenderBody) => {
  try {
    const {
      lead_enquiry_id,
      approx_value,
      source_name,
      sales_person_name,
      tender_reg_no,
      tender_identification_no,
      tender_name,
      tender_issue_date,
      tender_due_date,
      tender_type,
      estimated_value,
      industry_sector,
      updated_by,
      lead_tender_id,
    } = body;
    let result = null;

    const leadTenderExist = await leadTenderDao.getById(lead_tender_id);

    if (lead_enquiry_id) {
      const leadEnquiryExist = await leadEnquiryDao.getById(lead_enquiry_id);
      if (!leadEnquiryExist) {
        result = {
          message: 'lead_enquiry_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (leadTenderExist) {
      const leadTenderDetails = await leadTenderDao.edit(
        lead_enquiry_id,
        approx_value,
        source_name,
        sales_person_name,
        tender_reg_no,
        tender_identification_no,
        tender_name,
        tender_issue_date,
        tender_due_date,
        tender_type,
        estimated_value,
        industry_sector,
        updated_by,
        lead_tender_id
      );
      result = { message: 'success', status: true, data: leadTenderDetails };
      return result;
    } else {
      result = {
        message: 'lead_tender_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in leadTender service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get LeadTender By LeadTenderId
 * @param leadTenderId
 * @returns
 */
const getById = async (leadTenderId: number) => {
  try {
    let result = null;
    const leadTenderData = await leadTenderDao.getById(leadTenderId);
    if (leadTenderData) {
      result = { message: 'success', status: true, data: leadTenderData };
      return result;
    } else {
      result = {
        message: 'lead_tender_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById leadTender service : ', error);
    throw error;
  }
};

/**
 * Method to Get All LeadTender's
 * @returns
 */
const getAllLeadTender = async () => {
  try {
    const result = await leadTenderDao.getAll();
    const leadTenderData = { message: 'success', status: true, data: result };
    return leadTenderData;
  } catch (error) {
    console.log(
      'Error occurred in getAllLeadTender leadTender service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete leadTender
 * @param leadTenderId
 */
const deleteLeadTender = async (leadTenderId: number) => {
  try {
    const leadTenderExist = await leadTenderDao.getById(leadTenderId);
    if (!leadTenderExist) {
      const result = {
        status: false,
        message: 'lead_tender_id does not exist',
        data: null,
      };
      return result;
    }

    const data = await leadTenderDao.deleteLeadTender(leadTenderId);
    if (data) {
      const result = {
        status: true,
        message: 'LeadTender Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this leadTender',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteLeadTender leadTender service : ',
      error
    );
    throw error;
  }
};

export {
  createLeadTender,
  updateLeadTender,
  getAllLeadTender,
  getById,
  deleteLeadTender,
};
