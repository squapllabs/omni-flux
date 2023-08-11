import prisma from '../utils/prisma';
import { siteExpenseDetails } from '../interfaces/siteExpense.Interface';

const add = async (
  site_id: number,
  project_id: number,
  employee_name: string,
  employee_id: string,
  employee_phone: string,
  purpose: string,
  department: string,
  designation: string,
  start_date: Date,
  end_date: Date,
  created_by: bigint,
  site_expense_details: Array<siteExpenseDetails>,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;

    const siteExpense = await transaction.site_expense.create({
      data: {
        site_id,
        project_id,
        employee_name,
        employee_id,
        employee_phone,
        purpose,
        department,
        designation,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        is_delete: is_delete,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
      },
    });

    const newSiteExpenseId = siteExpense.site_expense_id;
    const siteExpenseDetailsData = [];

    for (const siteExpenseDetail of site_expense_details) {
      const description = siteExpenseDetail.description;
      const air_transport = siteExpenseDetail.air_transport;
      const fuel = siteExpenseDetail.fuel;
      const labour_advance = siteExpenseDetail.labour_advance;
      const phone_stationary = siteExpenseDetail.phone_stationary;
      const food_snacks = siteExpenseDetail.food_snacks;
      const purchase_service = siteExpenseDetail.purchase_service;
      const others = siteExpenseDetail.others;
      const total = siteExpenseDetail.total;
      const bill_details = siteExpenseDetail.bill_details;

      const newSiteExpenseDetail =
        await transaction.site_expense_details.create({
          data: {
            site_expense_id: newSiteExpenseId,
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
            created_by,
            created_date: currentDate,
            updated_date: currentDate,
          },
        });

      siteExpenseDetailsData.push(newSiteExpenseDetail);
    }

    const result = {
      site_expense: siteExpense,
      site_expense_details: siteExpenseDetailsData,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in siteExpenseDao add', error);
    throw error;
  }
};

const edit = async (
  site_id: number,
  project_id: number,
  employee_name: string,
  employee_id: string,
  employee_phone: string,
  purpose: string,
  department: string,
  designation: string,
  start_date: Date,
  end_date: Date,
  updated_by: bigint,
  site_expense_id: number,
  site_expense_details: Array<siteExpenseDetails>,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const siteExpense = await transaction.site_expense.update({
      where: {
        site_expense_id: site_expense_id,
      },
      data: {
        site_id,
        project_id,
        employee_name,
        employee_id,
        employee_phone,
        purpose,
        department,
        designation,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        updated_by,
        updated_date: currentDate,
      },
    });

    const siteExpenseDetailsData = [];

    for (const siteExpenseDetail of site_expense_details) {
      const description = siteExpenseDetail.description;
      const air_transport = siteExpenseDetail.air_transport;
      const fuel = siteExpenseDetail.fuel;
      const labour_advance = siteExpenseDetail.labour_advance;
      const phone_stationary = siteExpenseDetail.phone_stationary;
      const food_snacks = siteExpenseDetail.food_snacks;
      const purchase_service = siteExpenseDetail.purchase_service;
      const others = siteExpenseDetail.others;
      const total = siteExpenseDetail.total;
      const bill_details = siteExpenseDetail.bill_details;
      const site_expense_details_id = siteExpenseDetail.site_expense_details_id;
      const is_delete = siteExpenseDetail.is_delete;

      if (site_expense_details_id) {
        if (is_delete === 'Y') {
          await transaction.site_expense_details.delete({
            where: {
              site_expense_details_id: site_expense_details_id,
            },
          });
        } else {
          const newSiteExpenseDetail =
            await transaction.site_expense_details.update({
              where: {
                site_expense_details_id: site_expense_details_id,
              },
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
                updated_by,
                updated_date: currentDate,
              },
            });

          siteExpenseDetailsData.push(newSiteExpenseDetail);
        }
      } else {
        const newSiteExpenseDetail =
          await transaction.site_expense_details.create({
            data: {
              site_expense_id: site_expense_id,
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
              created_by: updated_by,
              created_date: currentDate,
              updated_date: currentDate,
            },
          });

        siteExpenseDetailsData.push(newSiteExpenseDetail);
      }
    }

    const result = {
      site_expense: siteExpense,
      site_expense_details: siteExpenseDetailsData,
    };
    return result;

    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpenseDao edit', error);
    throw error;
  }
};

const getById = async (siteExpenseId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteExpense = await transaction.site_expense.findFirst({
      where: {
        site_expense_id: Number(siteExpenseId),
        is_delete: false,
      },
      include: {
        site_expense_details: true,
        site_data: {
          select: {
            name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
    });
    if (siteExpense) {
      siteExpense.site_expense_details.forEach((detail) => {
        detail.is_delete = 'N';
      });
    }
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpense getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteExpense = await transaction.site_expense.findMany({
      where: {
        is_delete: false,
      },
      include: {
        site_expense_details: true,
        site_data: {
          select: {
            name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpense getAll dao', error);
    throw error;
  }
};

const deleteSiteExpense = async (
  siteExpenseId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const siteExpense = await transaction.site_expense.update({
      where: {
        site_expense_id: Number(siteExpenseId),
      },
      data: {
        is_delete: true,
      },
    });
    return siteExpense;
  } catch (error) {
    console.log('Error occurred in siteExpense deleteSiteExpense dao', error);
    throw error;
  }
};

const searchSiteExpense = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterSiteExpense;
    const siteExpense = await transaction.site_expense.findMany({
      where: filter,
      include: {
        site_expense_details: true,
        site_data: {
          select: {
            name: true,
          },
        },
        project_data: {
          select: {
            project_name: true,
          },
        },
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });

    const siteExpenseCount = await transaction.site_expense.count({
      where: filter,
    });
    const siteExpenseData = {
      count: siteExpenseCount,
      data: siteExpense,
    };
    return siteExpenseData;
  } catch (error) {
    console.log(
      'Error occurred in Site Expense dao : searchSiteExpense ',
      error
    );
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteSiteExpense,
  searchSiteExpense,
};
