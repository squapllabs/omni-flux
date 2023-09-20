import prisma from '../utils/prisma';
import common from './common/utils.dao';

const add = async (
  project_id: number,
  site_id: number,
  site_engineer_id: number,
  item_count: number,
  stock_outward_date: Date,
  created_by: number,
  stock_outward_details,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_stock_outward_date = stock_outward_date
      ? new Date(stock_outward_date)
      : null;

    const outwardIdGeneratorQuery = `select concat('STO',DATE_PART('year', CURRENT_DATE),'00',nextval('sto_sequence')::text) as stock_outward_sequence`;

    const outward_id = await common.customQueryExecutor(
      outwardIdGeneratorQuery
    );

    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockOutward = await transaction.stock_outward.create({
      data: {
        outward_id: outward_id[0].stock_outward_sequence,
        project_id,
        site_id,
        site_engineer_id,
        item_count,
        stock_outward_date: formatted_stock_outward_date,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });

    const new_stock_outward_id = stockOutward?.stock_outward_id;

    const stockOutwardDetailsData = [];

    for (const stock_outward_detail of stock_outward_details) {
      const item_id = stock_outward_detail?.item_id;
      const outward_quantity = stock_outward_detail?.outward_quantity;
      const uom_id = stock_outward_detail?.uom_id;
      const is_delete = stock_outward_detail?.is_delete;

      if (is_delete === false) {
        const stockOutwardDetails =
          await transaction.stock_outward_details.create({
            data: {
              stock_outward_id: new_stock_outward_id,
              item_id,
              outward_quantity,
              uom_id,
              created_by,
              created_date: currentDate,
              updated_date: currentDate,
              is_delete: false,
            },
          });
        stockOutwardDetailsData.push(stockOutwardDetails);
      }
    }

    const stockOutwardData = {
      stock_outward: stockOutward,
      stock_outward_details: stockOutwardDetailsData,
    };

    return stockOutwardData;
  } catch (error) {
    console.log('Error occurred in stockOutwardDao add', error);
    throw error;
  }
};

const edit = async (
  project_id: number,
  site_id: number,
  site_engineer_id: number,
  item_count: number,
  stock_outward_date: Date,
  updated_by: number,
  stock_outward_details,
  stock_outward_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_stock_outward_date = stock_outward_date
      ? new Date(stock_outward_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockOutward = await transaction.stock_outward.update({
      where: {
        stock_outward_id: Number(stock_outward_id),
      },
      data: {
        project_id,
        site_id,
        site_engineer_id,
        item_count,
        stock_outward_date: formatted_stock_outward_date,
        updated_by,
        updated_date: currentDate,
      },
    });

    const stockOutwardDetailsData = [];

    for (const stock_outward_detail of stock_outward_details) {
      const item_id = stock_outward_detail?.item_id;
      const outward_quantity = stock_outward_detail?.outward_quantity;
      const uom_id = stock_outward_detail?.uom_id;
      const is_delete = stock_outward_detail?.is_delete;
      const stock_outward_details_id =
        stock_outward_detail?.stock_outward_details_id;

      if (stock_outward_details_id) {
        if (is_delete === false) {
          const stockOutwardDetails =
            await transaction.stock_outward_details.update({
              where: {
                stock_outward_details_id: Number(stock_outward_details_id),
              },
              data: {
                stock_outward_id: stock_outward_id,
                item_id,
                outward_quantity,
                uom_id,
                updated_by,
                updated_date: currentDate,
              },
            });
          stockOutwardDetailsData.push(stockOutwardDetails);
        } else if (is_delete === true) {
          await transaction.stock_outward_details.update({
            where: {
              stock_outward_details_id: Number(stock_outward_details_id),
            },
            data: {
              is_delete: true,
            },
          });
        }
      } else {
        if (is_delete === false) {
          const stockOutwardDetails =
            await transaction.stock_outward_details.create({
              data: {
                stock_outward_id: stock_outward_id,
                item_id,
                outward_quantity,
                uom_id,
                created_by: updated_by,
                created_date: currentDate,
                updated_date: currentDate,
                is_delete: false,
              },
            });
          stockOutwardDetailsData.push(stockOutwardDetails);
        }
      }
    }

    const stockOutwardData = {
      stock_outward: stockOutward,
      stock_outward_details: stockOutwardDetailsData,
    };

    return stockOutwardData;
  } catch (error) {
    console.log('Error occurred in stockOutwardDao edit', error);
    throw error;
  }
};

const getById = async (stockOutwardId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockOutward = await transaction.stock_outward.findFirst({
      where: {
        stock_outward_id: Number(stockOutwardId),
        is_delete: false,
      },
      include: {
        project_data: true,
        site_data: true,
        site_engineer_data: { select: { first_name: true, last_name: true } },
        stock_outward_details: {
          where: { is_delete: false },
          include: {
            item_data: true,
            uom_data: { select: { name: true } },
          },
          orderBy: [{ updated_date: 'desc' }],
        },
      },
    });
    return stockOutward;
  } catch (error) {
    console.log('Error occurred in stockOutward getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockOutward = await transaction.stock_outward.findMany({
      where: {
        is_delete: false,
      },
      include: {
        project_data: true,
        site_data: true,
        site_engineer_data: { select: { first_name: true, last_name: true } },
        stock_outward_details: {
          where: { is_delete: false },
          include: { item_data: true, uom_data: { select: { name: true } } },
          orderBy: [{ updated_date: 'desc' }],
        },
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return stockOutward;
  } catch (error) {
    console.log('Error occurred in stockOutward getAll dao', error);
    throw error;
  }
};

const deleteStockOutward = async (
  stockOutwardId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const stockOutward = await transaction.stock_outward.update({
      where: {
        stock_outward_id: Number(stockOutwardId),
      },
      data: {
        is_delete: true,
      },
    });
    return stockOutward;
  } catch (error) {
    console.log('Error occurred in stockOutward deleteStockOutward dao', error);
    throw error;
  }
};

const searchStockOutward = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterStockOutward;
    const stockOutward = await transaction.stock_outward.findMany({
      where: filter,
      include: {
        project_data: true,
        site_data: true,
        site_engineer_data: { select: { first_name: true, last_name: true } },
        stock_outward_details: {
          where: { is_delete: false },
          include: { item_data: true, uom_data: { select: { name: true } } },
          orderBy: [{ updated_date: 'desc' }],
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
    const stockOutwardCount = await transaction.stock_outward.count({
      where: filter,
    });
    const stockOutwardData = {
      count: stockOutwardCount,
      data: stockOutward,
    };
    return stockOutwardData;
  } catch (error) {
    console.log(
      'Error occurred in stockOutward dao : searchStockOutward',
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
  deleteStockOutward,
  searchStockOutward,
};
