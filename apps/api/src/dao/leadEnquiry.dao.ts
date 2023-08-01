import prisma from '../utils/prisma';

const add = async (
  lead_type: string,
  lead_code: string,
  client_id: number,
  client_level: number,
  client_contact_name: string,
  client_contact_email: string,
  client_contact_phone: string,
  doc_url: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadEnquiry = await transaction.lead_enquiry.create({
      data: {
        lead_type,
        lead_code,
        client_id,
        client_level,
        client_contact_name,
        client_contact_email,
        client_contact_phone,
        doc_url,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return leadEnquiry;
  } catch (error) {
    console.log('Error occurred in leadEnquiryDao add', error);
    throw error;
  }
};

const edit = async (
  lead_type: string,
  lead_code: string,
  client_id: number,
  client_level: number,
  client_contact_name: string,
  client_contact_email: string,
  client_contact_phone: string,
  doc_url: string,
  updated_by: number,
  lead_enquiry_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadEnquiry = await transaction.lead_enquiry.update({
      where: {
        lead_enquiry_id: lead_enquiry_id,
      },
      data: {
        lead_type,
        lead_code,
        client_id,
        client_level,
        client_contact_name,
        client_contact_email,
        client_contact_phone,
        doc_url,
        updated_by,
        updated_date: currentDate,
      },
    });
    return leadEnquiry;
  } catch (error) {
    console.log('Error occurred in leadEnquiryDao edit', error);
    throw error;
  }
};

const getById = async (leadEnquiryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadEnquiry = await transaction.lead_enquiry.findFirst({
      where: {
        lead_enquiry_id: Number(leadEnquiryId),
        is_delete: false,
      },
      include: {
        client: true,
        lead_products: true,
        lead_tenders: true,
      },
    });
    return leadEnquiry;
  } catch (error) {
    console.log('Error occurred in leadEnquiry getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadEnquiry = await transaction.lead_enquiry.findMany({
      where: {
        is_delete: false,
      },
      include: {
        client: true,
        lead_products: true,
        lead_tenders: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return leadEnquiry;
  } catch (error) {
    console.log('Error occurred in leadEnquiry getAll dao', error);
    throw error;
  }
};

const deleteLeadEnquiry = async (
  leadEnquiryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadEnquiry = await transaction.lead_enquiry.update({
      where: {
        lead_enquiry_id: Number(leadEnquiryId),
      },
      data: {
        is_delete: true,
      },
    });
    return leadEnquiry;
  } catch (error) {
    console.log('Error occurred in leadEnquiry deleteLeadEnquiry dao', error);
    throw error;
  }
};

const getByClientId = async (clientId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadEnquiry = await transaction.lead_enquiry.findFirst({
      where: {
        client_id: Number(clientId),
        is_delete: false,
      },
    });
    return leadEnquiry;
  } catch (error) {
    console.log('Error occurred in leadEnquiry getByClientId dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteLeadEnquiry,
  getByClientId,
};
