import prisma from '../utils/prisma';

const add = async (
  lead_enquiry_id: number,
  approx_value: number,
  source_name: string,
  sales_person_name: number,
  tender_reg_no: string,
  tender_identification_no: string,
  tender_name: string,
  tender_issue_date: Date,
  tender_due_date: Date,
  tender_type: string,
  estimated_value: number,
  industry_sector: string,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadTender = await transaction.lead_tender.create({
      data: {
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
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return leadTender;
  } catch (error) {
    console.log('Error occurred in leadTenderDao add', error);
    throw error;
  }
};

const edit = async (
  lead_enquiry_id: number,
  approx_value: number,
  source_name: string,
  sales_person_name: number,
  tender_reg_no: string,
  tender_identification_no: string,
  tender_name: string,
  tender_issue_date: Date,
  tender_due_date: Date,
  tender_type: string,
  estimated_value: number,
  industry_sector: string,
  updated_by: number,
  lead_tender_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadTender = await transaction.lead_tender.update({
      where: {
        lead_tender_id: lead_tender_id,
      },
      data: {
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
        updated_date: currentDate,
      },
    });
    return leadTender;
  } catch (error) {
    console.log('Error occurred in leadTenderDao edit', error);
    throw error;
  }
};

const getById = async (leadTenderId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadTender = await transaction.lead_enquiry_tender.findFirst({
      where: {
        lead_tender_id: Number(leadTenderId),
        is_delete: false,
      },
    });
    return leadTender;
  } catch (error) {
    console.log('Error occurred in leadTender getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadTender = await transaction.lead_tender.findMany({
      where: {
        is_delete: false,
      },
      include: {
        lead_enquiry: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return leadTender;
  } catch (error) {
    console.log('Error occurred in leadTender getAll dao', error);
    throw error;
  }
};

const deleteLeadTender = async (leadTenderId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadTender = await transaction.lead_tender.update({
      where: {
        lead_tender_id: Number(leadTenderId),
      },
      data: {
        is_delete: true,
      },
    });
    return leadTender;
  } catch (error) {
    console.log('Error occurred in leadTender deleteLeadTender dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteLeadTender,
};
