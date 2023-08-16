import prisma from '../utils/prisma';

const add = async (
  description: string,
  air_transport: number,
  fuel: number,
  labour_advance: number,
  phone_stationary: number,
  food_snacks: number,
  purchase_service: number,
  others: number,
  total: number,
  bill_details: JSON,
  site_expense_id: number,
  created_by: bigint,
  status: string,
  comments: string,
  progressed_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const siteExpenseDetail = await transaction.site_expense_details.create({
      data: {
        description,
        air_transport,
        fuel,
        labour_advance,
        phone_stationary,
        food_snacks,
        purchase_service,
        others,
        total,
        bill_details,
        site_expense_id,
        created_date: currentDate,
        updated_date: currentDate,
        created_by,
        status,
        comments,
        progressed_date:
          progressed_by || status || comments ? currentDate : null,
        progressed_by,
        is_delete: is_delete,
      },
    });

    return siteExpenseDetail;
  } catch (error) {
    console.log('Error occurred in siteExpensDetailsDao add', error);
    throw error;
  }
};

export default {
  add,
};
