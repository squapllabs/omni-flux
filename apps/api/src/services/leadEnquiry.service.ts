import leadEnquiryDao from '../dao/leadEnquiry.dao';
import {
  createLeadEnquiryBody,
  updateLeadEnquiryBody,
} from '../interfaces/leadEnquiry.Interface';
import leadProductDao from '../dao/leadProduct.dao';
import leadTenderDao from '../dao/leadTender.dao';
import clientDao from '../dao/client.dao';
import prisma from '../utils/prisma';

/**
 * Method to Create a New LeadEnquiry
 * @param body
 * @returns
 */
const createLeadEnquiry = async (body: createLeadEnquiryBody) => {
  try {
    const {
      lead_type,
      lead_code,
      client_id,
      client_level,
      client_contact_name,
      client_contact_email,
      client_contact_phone,
      doc_url,
      created_by,
      source_name,
      probability,
      our_remarks,
      client_remark,
      approx_value,
      sales_person_name,
      tender_reg_no,
      tender_identification_no,
      tender_name,
      tender_issue_date,
      tender_due_date,
      tender_type,
      estimated_value,
      industry_sector,
    } = body;
    let result = null;
    const leadEquiryData = [];

    if (client_id) {
      const clientExist = await clientDao.getById(client_id);
      if (!clientExist) {
        result = {
          message: 'client_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    result = await prisma
      .$transaction(async (prisma) => {
        const leadEnquiryDetails = await leadEnquiryDao.add(
          lead_type,
          lead_code,
          client_id,
          client_level,
          client_contact_name,
          client_contact_email,
          client_contact_phone,
          doc_url,
          created_by,
          prisma
        );

        leadEquiryData.push({ leadEnquiryDetails: leadEnquiryDetails });

        if (leadEnquiryDetails) {
          if (lead_type === 'Product') {
            const leadProductDetails = await leadProductDao.add(
              leadEnquiryDetails?.lead_enquiry_id,
              source_name,
              probability,
              our_remarks,
              client_remark,
              approx_value,
              sales_person_name,
              created_by,
              prisma
            );

            leadEquiryData.push({ leadProductDetails: leadProductDetails });
          } else if (lead_type === 'Tender') {
            const leadTenderDetails = await leadTenderDao.add(
              leadEnquiryDetails?.lead_enquiry_id,
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
              prisma
            );

            leadEquiryData.push({ leadTenderDetails: leadTenderDetails });
          }
        }

        return leadEquiryData;
      })
      .then((data) => {
        console.log('Successfully Lead Enquiry Data Returned ', data);
        const newLeadEnquiryData = {
          message: 'success',
          status: true,
          data: data,
        };
        return newLeadEnquiryData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in leadEnquiry service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing LeadEnquiry
 * @param body
 * @returns
 */
const updateLeadEnquiry = async (body: updateLeadEnquiryBody) => {
  try {
    const {
      lead_type,
      lead_code,
      client_id,
      client_level,
      client_contact_name,
      client_contact_email,
      client_contact_phone,
      doc_url,
      lead_enquiry_id,
      source_name,
      probability,
      our_remarks,
      client_remark,
      approx_value,
      sales_person_name,
      updated_by,
      lead_product_id,
      tender_reg_no,
      tender_identification_no,
      tender_name,
      tender_issue_date,
      tender_due_date,
      tender_type,
      estimated_value,
      industry_sector,
      lead_tender_id,
    } = body;
    let result = null;
    const leadEquiryData = [];
    const leadEnquiryExist = await leadEnquiryDao.getById(lead_enquiry_id);

    if (client_id) {
      const clientExist = await clientDao.getById(client_id);
      if (!clientExist) {
        const result = {
          message: 'client_id does not exist',
          status: false,
          data: null,
        };
        return result;
      }
    }

    if (leadEnquiryExist) {
      result = await prisma
        .$transaction(async (prisma) => {
          const leadEnquiryDetails = await leadEnquiryDao.edit(
            lead_type,
            lead_code,
            client_id,
            client_level,
            client_contact_name,
            client_contact_email,
            client_contact_phone,
            doc_url,
            updated_by,
            lead_enquiry_id,
            prisma
          );

          leadEquiryData.push({ leadEnquiryDetails: leadEnquiryDetails });

          if (leadEnquiryDetails) {
            if (lead_type === 'Product') {
              const leadProductDetails = await leadProductDao.edit(
                lead_enquiry_id,
                source_name,
                probability,
                our_remarks,
                client_remark,
                approx_value,
                sales_person_name,
                updated_by,
                lead_product_id,
                prisma
              );

              leadEquiryData.push({ leadProductDetails: leadProductDetails });
            } else if (lead_type === 'Tender') {
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
                lead_tender_id,
                prisma
              );

              leadEquiryData.push({ leadTenderDetails: leadTenderDetails });
            }
          }

          return leadEquiryData;
        })
        .then((data) => {
          console.log('Successfully Lead Enquiry Data Returned ', data);
          const newLeadEnquiryData = {
            message: 'success',
            status: true,
            data: data,
          };
          return newLeadEnquiryData;
        })
        .catch((error: string) => {
          console.log('Failure, ROLLBACK was executed', error);
          throw error;
        });
      return result;
    } else {
      result = {
        message: 'lead_enquiry_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in leadEnquiry service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get LeadEnquiry By LeadEnquiryId
 * @param leadEnquiryId
 * @returns
 */
const getById = async (leadEnquiryId: number) => {
  try {
    let result = null;
    const leadEnquiryData = await leadEnquiryDao.getById(leadEnquiryId);
    if (leadEnquiryData) {
      result = { message: 'success', status: true, data: leadEnquiryData };
      return result;
    } else {
      result = {
        message: 'lead_enquiry_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById leadEnquiry service : ', error);
    throw error;
  }
};

/**
 * Method to Get All LeadEnquiry's
 * @returns
 */
const getAllLeadEnquiry = async () => {
  try {
    const result = await leadEnquiryDao.getAll();
    const leadEnquiryData = { message: 'success', status: true, data: result };
    return leadEnquiryData;
  } catch (error) {
    console.log(
      'Error occurred in getAllLeadEnquiry leadEnquiry service : ',
      error
    );
    throw error;
  }
};

/**
 * Method to delete leadEnquiry
 * @param leadEnquiryId
 */
const deleteLeadEnquiry = async (leadEnquiryId: number) => {
  try {
    const leadEnquiryExist = await leadEnquiryDao.getById(leadEnquiryId);
    if (!leadEnquiryExist) {
      const result = {
        status: false,
        message: 'lead_enquiry_id does not exist',
        data: null,
      };
      return result;
    }

    if (leadEnquiryExist.lead_products.length === 0) {
      const result = {
        status: false,
        message:
          'Unable to delete.The lead_enquiry_id is mapped in lead_product table',
        data: null,
      };
      return result;
    }

    if (leadEnquiryExist.lead_tenders.length === 0) {
      const result = {
        status: false,
        message:
          'Unable to delete.The lead_enquiry_id is mapped in lead_tender table',
        data: null,
      };
      return result;
    }

    const data = await leadEnquiryDao.deleteLeadEnquiry(leadEnquiryId);
    if (data) {
      const result = {
        status: true,
        message: 'LeadEnquiry Data Deleted Successfully',
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this leadEnquiry',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in deleteLeadEnquiry leadEnquiry service : ',
      error
    );
    throw error;
  }
};

export {
  createLeadEnquiry,
  updateLeadEnquiry,
  getAllLeadEnquiry,
  getById,
  deleteLeadEnquiry,
};
