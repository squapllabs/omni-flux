import { grnDetailsBody } from '../interfaces/grn.interface';
import prisma from '../utils/prisma';

const add = async (
  project_id: number,
  purchase_order_id: number,
  goods_received_by: number,
  goods_received_date: Date,
  invoice_id: string,
  notes: string,
  bill_details: JSON,
  grn_status: string,
  created_by: number,
  grn_details: Array<grnDetailsBody>,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_goods_received_date = goods_received_date
      ? new Date(goods_received_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const grn = await transaction.grn.create({
      data: {
        project_id,
        purchase_order_id,
        goods_received_by,
        goods_received_date: formatted_goods_received_date,
        invoice_id,
        notes,
        bill_details,
        grn_status,
        created_by,
        created_date: currentDate,
      },
    });

    const new_grn_id = grn?.grn_id;
    const grnDetailsData = [];
    for await (const grnDetail of grn_details) {
      const {
        item_id,
        currently_received_quantity,
        accepted_quantity,
        rejected_quantity,
        notes,
      } = grnDetail;

      const grnDetails = await transaction.grn_details.create({
        data: {
          grn_id: new_grn_id,
          item_id,
          received_quantity: currently_received_quantity,
          accepted_quantity: accepted_quantity || currently_received_quantity,
          rejected_quantity: rejected_quantity || 0,
          notes,
          created_by,
        },
      });
      grnDetailsData.push(grnDetails);
    }

    const grnData = {
      grn: grn,
      grn_details: grnDetailsData,
    };
    return grnData;
  } catch (error) {
    console.log('Error occurred in grnDao add', error);
    throw error;
  }
};

const getById = async (grnId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const grn = await transaction.grn.findFirst({
      where: {
        grn_id: Number(grnId),
      },
      include: {
        project_data: true,
        purchase_order_data: {
          include: { vendor_data: { select: { vendor_name: true } } },
        },
        goods_received_by_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        grn_details: {
          include: {
            item_data: {
              include: { uom: true },
            },
          },
          orderBy: { created_date: 'asc' },
        },
      },
    });
    return grn;
  } catch (error) {
    console.log('Error occurred in grn getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const grn = await transaction.grn.findMany({
      include: {
        project_data: true,
        purchase_order_data: {
          include: { vendor_data: { select: { vendor_name: true } } },
        },
        goods_received_by_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        grn_details: {
          include: {
            item_data: {
              include: { uom: true },
            },
          },
          orderBy: { created_date: 'asc' },
        },
      },
      orderBy: [
        {
          created_date: 'asc',
        },
      ],
    });
    return grn;
  } catch (error) {
    console.log('Error occurred in grn getAll dao', error);
    throw error;
  }
};

const searchGrn = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterGrn;
    let grnDataAvailability = [];
    if (filter.AND && filter.AND[0]?.purchase_order_id) {
      grnDataAvailability = await transaction.grn.findMany({
        where: {
          purchase_order_id: filter.AND[0]?.purchase_order_id,
        },
      });
    } else {
      grnDataAvailability = await transaction.grn.findMany({});
    }
    if (grnDataAvailability.length > 0) {
      const grn = await transaction.grn.findMany({
        where: filter,
        include: {
          project_data: true,
          purchase_order_data: {
            include: { vendor_data: { select: { vendor_name: true } } },
          },
          goods_received_by_data: {
            select: {
              first_name: true,
              last_name: true,
            },
          },
          grn_details: {
            include: {
              item_data: {
                include: { uom: true },
              },
            },
            orderBy: { created_date: 'asc' },
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
      const grnCount = await transaction.grn.count({
        where: filter,
      });
      const grnData = {
        count: grnCount,
        data: grn,
      };
      return grnData;
    } else {
      return {
        count: -1,
        data: null,
      };
    }
  } catch (error) {
    console.log('Error occurred in grn dao : searchGrn', error);
    throw error;
  }
};

const getByPurchaseOrderId = async (
  purchase_order_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const grn = await transaction.grn.findMany({
      where: {
        purchase_order_id: Number(purchase_order_id),
      },
      include: {
        project_data: true,
        purchase_order_data: {
          include: { vendor_data: { select: { vendor_name: true } } },
        },
        goods_received_by_data: {
          select: {
            first_name: true,
            last_name: true,
          },
        },
        grn_details: {
          include: {
            item_data: {
              include: { uom: true },
            },
          },
          orderBy: { created_date: 'asc' },
        },
      },
      orderBy: [{ created_date: 'asc' }],
    });
    return grn;
  } catch (error) {
    console.log('Error occurred in grn getByPurchaseOrderId dao', error);
    throw error;
  }
};

export default {
  add,
  getById,
  getAll,
  searchGrn,
  getByPurchaseOrderId,
};
