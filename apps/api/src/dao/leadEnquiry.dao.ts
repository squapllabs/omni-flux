import prisma from '../utils/prisma';

const add = async (
  lead_type: string,
  lead_code: string,
  client: number,
  client_level: number,
  client_contact_name: string,
  client_contact_email: string,
  client_contact_phone: string,
  our_remarks: string,
  client_remark: string,
  doc_url: string,
  created_by: number,
  status_remarks: string,
  source_name: string,
  status: string,
  probability: number,
  approx_value: number,
  sales_person_name: number,
  tender_reg_no: string,
  tender_identification_no: string,
  tender_name: string,
  tender_issue_date: Date,
  tender_due_date: Date,
  tender_type: string,
  estimated_value: number,
  industry_sector: number,
  product_id: number,
  quantity: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_tender_issue_date = tender_issue_date
      ? new Date(tender_issue_date)
      : null;
    const formatted_tender_due_date = tender_due_date
      ? new Date(tender_due_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const leadEnquiry = await transaction.lead_enquiry.create({
      data: {
        lead_type,
        lead_code,
        client_info: { connect: { client_id: client } },
        client_level_info: { connect: { master_data_id: client_level } },
        client_contact_name,
        client_contact_email,
        client_contact_phone,
        our_remarks,
        client_remark,
        doc_url,
        status,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
        status_remarks,
      },
    });

    const newLeadEnquiryId = leadEnquiry.lead_enquiry_id;

    let leadEnquiryProduct: object,
      leadEnquiryTender: object,
      leadEnquiryProductItem: object;

    if (lead_type === 'Product') {
      leadEnquiryProduct = await transaction.lead_enquiry_product.create({
        data: {
          source_name,
          probability_details: {
            connect: { master_data_id: probability },
          },
          approx_value,
          sales_person_details: {
            connect: { user_id: sales_person_name },
          },
          lead_enquiry: {
            connect: {
              lead_enquiry_id: newLeadEnquiryId,
            },
          },
          created_by,
          created_date: currentDate,
          updated_date: currentDate,
          is_delete: is_delete,
        },
      });
      leadEnquiryProductItem =
        await transaction.lead_enquiry_product_item.create({
          data: {
            lead_enquiry: {
              connect: {
                lead_enquiry_id: newLeadEnquiryId,
              },
            },
            product: {
              connect: {
                item_id: product_id,
              },
            },
            quantity,
            created_date: currentDate,
            updated_date: currentDate,
            created_by,
          },
        });
    } else if (lead_type === 'Tender') {
      leadEnquiryTender = await transaction.lead_enquiry_tender.create({
        data: {
          tender_reg_no,
          tender_identification_no,
          tender_name,
          tender_issue_date: formatted_tender_issue_date,
          tender_due_date: formatted_tender_due_date,
          tender_type,
          estimated_value,
          industry_sector_data: {
            connect: { master_data_id: industry_sector },
          },
          lead_enquiry: {
            connect: {
              lead_enquiry_id: newLeadEnquiryId,
            },
          },
          created_by,
          created_date: currentDate,
          updated_date: currentDate,
          is_delete: is_delete,
        },
      });
    }

    return {
      lead_enquiry: leadEnquiry,
      lead_enquiry_product: leadEnquiryProduct,
      lead_enquiry_tender: leadEnquiryTender,
      lead_enquiry_product_item: leadEnquiryProductItem,
    };
  } catch (error) {
    console.log('Error occurred in leadEnquiryDao add', error);
    throw error;
  }
};

const edit = async (
  lead_type: string,
  client: number,
  client_level: number,
  client_contact_name: string,
  client_contact_email: string,
  client_contact_phone: string,
  our_remarks: string,
  client_remark: string,
  doc_url: string,
  updated_by: number,
  source_name: string,
  status: string,
  status_remarks: string,
  lead_enquiry_id: number,
  probability: number,
  approx_value: number,
  sales_person_name: number,
  tender_reg_no: string,
  tender_identification_no: string,
  tender_name: string,
  tender_issue_date: Date,
  tender_due_date: Date,
  tender_type: string,
  estimated_value: number,
  industry_sector: number,
  product_id: number,
  quantity: number,
  lead_product_id: number,
  lead_tender_id: number,
  lead_enquiry_product_item_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_tender_issue_date = tender_issue_date
      ? new Date(tender_issue_date)
      : null;
    const formatted_tender_due_date = tender_due_date
      ? new Date(tender_due_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const leadEnquiry = await transaction.lead_enquiry.update({
      where: {
        lead_enquiry_id: Number(lead_enquiry_id),
      },
      data: {
        lead_type,
        client_info: { connect: { client_id: client } },
        client_level_info: { connect: { master_data_id: client_level } },
        client_contact_name,
        client_contact_email,
        client_contact_phone,
        our_remarks,
        client_remark,
        doc_url,
        status,
        updated_by,
        updated_date: currentDate,
        status_remarks,
      },
    });

    let leadEnquiryProduct: object,
      leadEnquiryTender: object,
      leadEnquiryProductItem: object;

    if (lead_type === 'Product') {
      leadEnquiryProduct = await transaction.lead_enquiry_product.update({
        where: { lead_product_id: Number(lead_product_id) },
        data: {
          source_name,
          probability_details: {
            connect: { master_data_id: probability },
          },
          approx_value,
          sales_person_details: {
            connect: { user_id: sales_person_name },
          },
          lead_enquiry: {
            connect: {
              lead_enquiry_id,
            },
          },
          updated_by,
          updated_date: currentDate,
        },
      });
      leadEnquiryProductItem =
        await transaction.lead_enquiry_product_item.update({
          where: {
            lead_enquiry_product_item_id: Number(lead_enquiry_product_item_id),
          },
          data: {
            lead_enquiry: {
              connect: {
                lead_enquiry_id,
              },
            },
            product: {
              connect: {
                item_id: product_id,
              },
            },
            quantity,
            updated_by,
            updated_date: currentDate,
          },
        });
    } else if (lead_type === 'Tender') {
      leadEnquiryTender = await transaction.lead_enquiry_tender.update({
        where: {
          lead_tender_id: Number(lead_tender_id),
        },
        data: {
          tender_reg_no,
          tender_identification_no,
          tender_name,
          tender_issue_date: formatted_tender_issue_date,
          tender_due_date: formatted_tender_due_date,
          tender_type,
          estimated_value,
          industry_sector_data: {
            connect: { master_data_id: industry_sector },
          },
          lead_enquiry: {
            connect: {
              lead_enquiry_id,
            },
          },
          updated_by,
          updated_date: currentDate,
        },
      });
    }

    return {
      lead_enquiry: leadEnquiry,
      lead_enquiry_product: leadEnquiryProduct,
      lead_enquiry_tender: leadEnquiryTender,
      lead_enquiry_product_item: leadEnquiryProductItem,
    };
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
        client_info: {
          select: {
            name: true,
          },
        },
        client_level_info: { select: { master_data_name: true } },
        lead_enquiry_product: {
          include: {
            sales_person_details: {
              select: {
                first_name: true,
                last_name: true,
              },
            },
            probability_details: {
              select: { master_data_name: true },
            },
          },
        },
        lead_enquiry_tenders: {
          include: {
            industry_sector_data: {
              select: { master_data_name: true },
            },
          },
        },
        lead_enquiry_product_item: {
          include: {
            product: {
              select: {
                item_name: true,
              },
            },
          },
        },
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
        client_info: {
          select: {
            name: true,
          },
        },
        client_level_info: { select: { master_data_name: true } },
        lead_enquiry_product: {
          include: {
            sales_person_details: {
              select: {
                first_name: true,
                last_name: true,
              },
            },
            probability_details: {
              select: { master_data_name: true },
            },
          },
        },
        lead_enquiry_tenders: {
          include: {
            industry_sector_data: {
              select: { master_data_name: true },
            },
          },
        },
        lead_enquiry_product_item: {
          include: {
            product: {
              select: {
                item_name: true,
              },
            },
          },
        },
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
        client: Number(clientId),
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
