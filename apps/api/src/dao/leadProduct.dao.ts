import prisma from '../utils/prisma';

const add = async (
  lead_enquiry_id: number,
  source_name: string,
  probability: number,
  approx_value: number,
  sales_person_name: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadProduct = await transaction.lead_product.create({
      data: {
        lead_enquiry_id,
        source_name,
        probability,
        approx_value,
        sales_person_name,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return leadProduct;
  } catch (error) {
    console.log('Error occurred in leadProductDao add', error);
    throw error;
  }
};

const edit = async (
  lead_enquiry_id: number,
  source_name: string,
  probability: number,
  approx_value: number,
  sales_person_name: number,
  updated_by: number,
  lead_product_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadProduct = await transaction.lead_product.update({
      where: {
        lead_product_id: lead_product_id,
      },
      data: {
        lead_enquiry_id,
        source_name,
        probability,
        approx_value,
        sales_person_name,
        updated_by,
        updated_date: currentDate,
      },
    });
    return leadProduct;
  } catch (error) {
    console.log('Error occurred in leadProductDao edit', error);
    throw error;
  }
};

const getById = async (leadProductId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadProduct = await transaction.lead_enquiry_product.findFirst({
      where: {
        lead_product_id: Number(leadProductId),
        is_delete: false,
      },
    });
    return leadProduct;
  } catch (error) {
    console.log('Error occurred in leadProduct getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadProduct = await transaction.lead_product.findMany({
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
    return leadProduct;
  } catch (error) {
    console.log('Error occurred in leadProduct getAll dao', error);
    throw error;
  }
};

const deleteLeadProduct = async (
  leadProductId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const leadProduct = await transaction.lead_product.update({
      where: {
        lead_product_id: Number(leadProductId),
      },
      data: {
        is_delete: true,
      },
    });
    return leadProduct;
  } catch (error) {
    console.log('Error occurred in leadProduct deleteLeadProduct dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteLeadProduct,
};
