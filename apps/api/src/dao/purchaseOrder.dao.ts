import db from '../utils/db';
import prisma from '../utils/prisma';
import customQueryExecutor from './common/utils.dao';

const add = async (
  purchase_request_id: number,
  vendor_id: number,
  order_date: Date,
  status: string,
  total_cost: number,
  order_remark: string,
  created_by: number,
  purchase_order_documents,
  payment_mode: string,
  payment_date: Date,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_order_date = order_date ? new Date(order_date) : null;
    const formatted_payment_date = payment_date ? new Date(payment_date) : null;

    const orderIdGeneratorQuery = `select concat('PO',DATE_PART('year', CURRENT_DATE),'00',nextval('po_sequence')::text) as order_id_sequence`;
    const order_id = await customQueryExecutor.customQueryExecutor(
      orderIdGeneratorQuery
    );
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.create({
      data: {
        purchase_request_id,
        vendor_id,
        order_date: formatted_order_date,
        status,
        total_cost,
        order_remark,
        purchase_order_documents,
        order_id: order_id[0].order_id_sequence,
        payment_mode,
        payment_date: formatted_payment_date,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrderDao add', error);
    throw error;
  }
};

const edit = async (
  purchase_request_id: number,
  vendor_id: number,
  order_date: Date,
  status: string,
  total_cost: number,
  order_remark: string,
  updated_by: number,
  purchase_order_documents,
  payment_mode: string,
  payment_date: Date,
  purchase_order_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_order_date = order_date ? new Date(order_date) : null;
    const formatted_payment_date = payment_date ? new Date(payment_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.update({
      where: {
        purchase_order_id: purchase_order_id,
      },
      data: {
        purchase_request_id,
        vendor_id,
        order_date: formatted_order_date,
        status,
        total_cost,
        order_remark,
        purchase_order_documents,
        payment_mode,
        payment_date: formatted_payment_date,
        updated_by,
        updated_date: currentDate,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrderDao edit', error);
    throw error;
  }
};

const getById = async (purchaseOrderId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.findFirst({
      where: {
        purchase_order_id: Number(purchaseOrderId),
        is_delete: false,
      },
      include: {
        purchase_request_data: { include: { indent_request_data: true } },
        vendor_data: true,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrder getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.findMany({
      where: {
        is_delete: false,
      },
      include: {
        purchase_request_data: { include: { indent_request_data: true } },
        vendor_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrder getAll dao', error);
    throw error;
  }
};

const deletePurchaseOrder = async (
  purchaseOrderId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.update({
      where: {
        purchase_order_id: Number(purchaseOrderId),
      },
      data: {
        is_delete: true,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrder deletePurchaseOrder dao',
      error
    );
    throw error;
  }
};

const searchPurchaseOrder = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterPurchaseOrder;
    const purchaseOrder = await transaction.purchase_order.findMany({
      where: filter,
      include: {
        purchase_request_data: {
          include: { indent_request_data: true, project_data: true },
        },
        vendor_data: true,
      },
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      skip: offset,
      take: limit,
    });
    const purchaseOrderCount = await transaction.purchase_order.count({
      where: filter,
    });
    const purchaseOrderData = {
      count: purchaseOrderCount,
      data: purchaseOrder,
    };
    return purchaseOrderData;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrder dao : searchPurchaseOrder',
      error
    );
    throw error;
  }
};

const createPurchaseOrderWithItem = async (
  purchase_request_id: number,
  vendor_id: number,
  order_date: Date,
  status: string,
  total_cost: number,
  order_remark: string,
  created_by: number,
  purchase_order_item,
  connectionObj = null
) => {
  let transaction;
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_order_date = order_date ? new Date(order_date) : null;
    transaction = connectionObj !== null ? connectionObj : prisma;
    const orderIdGeneratorQuery = `select concat('PO',DATE_PART('year', CURRENT_DATE),'00',nextval('po_sequence')::text) as order_id_sequence`;
    const order_id = await customQueryExecutor.customQueryExecutor(
      orderIdGeneratorQuery
    );

    const result = await transaction
      .$transaction(async (tx) => {
        const purchaseOrder = await tx.purchase_order.create({
          data: {
            purchase_request_id,
            vendor_id,
            order_date: formatted_order_date,
            status,
            total_cost,
            order_remark,
            created_by,
            order_id: order_id[0]?.order_id_sequence,
            created_date: currentDate,
            updated_date: currentDate,
            is_delete: is_delete,
          },
        });

        const new_purchase_order_id = purchaseOrder?.purchase_order_id;
        const purchaseOrderItemDetails = [];

        if (purchase_order_item.length > 0) {
          for (const value of purchase_order_item) {
            const item_id = value.item_id;
            const order_quantity = value.order_quantity;
            const unit_price = value.unit_price;

            const purchaseOrderItem = await tx.purchase_order_item.create({
              data: {
                purchase_order_id: new_purchase_order_id,
                item_id,
                order_quantity,
                unit_price,
                created_by,
                created_date: currentDate,
                updated_date: currentDate,
                is_delete: is_delete,
              },
            });
            purchaseOrderItemDetails.push(purchaseOrderItem);
          }
        }

        const purchaseOrderData = {
          purchase_order: purchaseOrder,
          purchase_order_item: purchaseOrderItemDetails,
        };

        return purchaseOrderData;
      })
      .then((data) => {
        console.log('Successfully Purchase Order Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderDao createPurchaseOrderWithItem',
      error
    );

    throw error;
  }
};

const getByPurchaseRequestId = async (
  purchaseRequestId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const purchaseOrder = await transaction.purchase_order.findFirst({
      where: {
        purchase_request_id: Number(purchaseRequestId),
        is_delete: false,
      },
      include: {
        purchase_request_data: {
          include: { indent_request_data: true, project_data: true },
        },
        vendor_data: true,
        purchase_order_item: {
          where: { is_delete: false },
          orderBy: [{ updated_date: 'desc' }],
          include: { item_data: true },
        },
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return purchaseOrder;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrder getByPurchaseRequestId dao',
      error
    );
    throw error;
  }
};

const updateStatusAndDocument = async (
  status: string,
  updated_by: number,
  purchase_order_documents,
  purchase_order_id: number,
  payment_mode: string,
  payment_date: Date,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const formatted_payment_date = payment_date ? new Date(payment_date) : null;
    const purchaseOrder = await transaction.purchase_order.update({
      where: {
        purchase_order_id: purchase_order_id,
      },
      data: {
        status,
        purchase_order_documents,
        updated_by,
        payment_mode,
        payment_date: formatted_payment_date,
        updated_date: currentDate,
      },
    });
    return purchaseOrder;
  } catch (error) {
    console.log(
      'Error occurred in purchaseOrderDao updateStatusAndDocument',
      error
    );
    throw error;
  }
};

const getPOStatistics = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const purchaseOrderStatisticsQuery = `with project_based_purchase_order_data as (
      select
        p.project_name,
        p.project_id,
        p.estimated_budget,
        p.actual_budget,
        COUNT(po.*) as total_po_count,
        SUM(case when po.status = 'Completed' then 1 else 0 end) as count_of_completed_po,
        SUM(case when po.status != 'Completed' then 1 else 0 end) as count_of_pending_po,
        SUM(case when po.status = 'Completed' then po.total_cost else 0 end) as total_cost_completed,
        SUM(case when po.status != 'Completed' then po.total_cost else 0 end) as total_cost_other_than_completed,
        SUM(po.total_cost) as total_purchase_order_cost
      from
        purchase_order po
      left join purchase_request pr on
        pr.purchase_request_id = po.purchase_request_id
      left join project p on
        p.project_id = pr.project_id
      where
        po.is_delete = false
      group by
        p.project_id,
        p.project_name,
        p.estimated_budget,
        p.actual_budget
      order by
        total_cost_other_than_completed desc
      limit 5
            ),
            
            purchase_order_statistics as (
      select
        SUM(case when po.status = 'Processing' then po.total_cost else 0 end) as total_cost_processing,
        SUM(case when po.status = 'Product Received' then po.total_cost else 0 end) as total_cost_product_received,
        SUM(case when po.status = 'Invoice' then po.total_cost else 0 end) as total_cost_invoice,
        SUM(case when po.status = 'Completed' then po.total_cost else 0 end) as total_cost_completed,
        SUM(case when po.status != 'Completed' then po.total_cost else 0 end) as total_cost_other_than_completed,
        SUM(po.total_cost) as total_purchase_order_cost,
        (
        select
          COUNT(*)
        from
          purchase_order
        where
          status = 'Completed'
          and is_delete = false
                    ) as completed_po,
        (
        select
          COUNT(*)
        from
          purchase_order
        where
          status != 'Completed'
          and is_delete = false
                    ) as pending_po
      from
        purchase_order po
      where
        po.is_delete = false
            ),
            
            vendor_count as (
      select
        count(v.*)as total_vendors,
        COUNT(case when v.is_delete = false then 1 end)::INTEGER as active_vendors,
        COUNT(case when v.is_delete = true then 1 end)::INTEGER as inactive_vendors
      from
        vendor v
            ),
            
            vendor_involved_in_purchase_order as (
      select
        COUNT(v.*)::INTEGER as total_vendor_involved_in_po
      from
        (
        select
          po.vendor_id
        from
          purchase_order po
        where
          po.is_delete = false
        group by
          po.vendor_id
                    ) as v
            ),
            
            dashboard_data as (
      select
        status.total_projects,
        status.active_projects,
        status.inactive_projects,
        status.inprogress_projects,
        status.completed_projects,
        status.draft_projects,
        project_list.top_projects
      from
        (
        select
          count(*) as total_projects,
          count(p.is_delete)filter (
        where
          is_delete = false)as active_projects,
          count(p.is_delete)filter (
        where
          is_delete = true)as inactive_projects,
          count(status)filter (
        where
          status = 'Inprogress')as inprogress_projects,
          count(status)filter (
        where
          status = 'Completed')as completed_projects,
          count(status)filter (
        where
          status = 'Draft')as draft_projects
        from
          project p)status
      join (
        select
          jsonb_agg(top_five.top_five_projects) as top_projects
        from
          (
          select
            jsonb_build_object('project_total_days',
                    (p.date_ended - p.date_started),
                    'days_completed',
                    (current_date - date_started ),
                    'project_name',
                    p.project_name,
                   'total_budget',
                    p.actual_budget) as top_five_projects
          from
            project p
          order by
            p.actual_budget desc
          limit 5) top_five)project_list on
        true
            )
            
            select
        pd_array.project_data as project_based_purchase_order_data,
        po_stats.total_purchase_order_statistics,
        vc.total_vendors::INTEGER,
        vc.active_vendors,
        vc.inactive_vendors,
        vipo.total_vendor_involved_in_po,
        dd.top_projects,
        dd.total_projects::INTEGER,
        dd.active_projects::INTEGER,
        dd.inactive_projects::INTEGER,
        dd.inprogress_projects::INTEGER,
        dd.completed_projects::INTEGER,
        dd.draft_projects::INTEGER
      from
        (
        select
          JSON_AGG(
                    JSON_BUILD_OBJECT(
                        'project_name', project_name,
                        'project_id', project_id,
                        'estimated_budget', estimated_budget,
                        'actual_budget', actual_budget,
                        'total_po_count', total_po_count::INTEGER,
                        'count_of_completed_po', count_of_completed_po::INTEGER,
                        'count_of_pending_po', count_of_pending_po::INTEGER,
                        'total_cost_completed', total_cost_completed,
                        'total_cost_other_than_completed', total_cost_other_than_completed,
                        'total_purchase_order_cost', total_purchase_order_cost
                    )
                ) as project_data
        from
          project_based_purchase_order_data
            ) as pd_array
      cross join (
        select
          JSON_BUILD_OBJECT(
                        'total_cost_processing', pos.total_cost_processing,
                        'total_cost_product_received', pos.total_cost_product_received,
                        'total_cost_invoice', pos.total_cost_invoice,
                        'total_cost_completed', pos.total_cost_completed,
                        'total_cost_other_than_completed', pos.total_cost_other_than_completed,
                        'total_purchase_order_cost', pos.total_purchase_order_cost,
                        'completed_po', pos.completed_po,
                        'pending_po', pos.pending_po
                    ) as total_purchase_order_statistics
        from
          purchase_order_statistics pos
            ) as po_stats
      cross join vendor_count vc
      cross join vendor_involved_in_purchase_order vipo
      cross join dashboard_data dd`;

    const purchaseOrder = await transaction.oneOrNone(
      purchaseOrderStatisticsQuery
    );
    return purchaseOrder;
  } catch (error) {
    console.log('Error occurred in purchaseOrder getPOStatistics dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deletePurchaseOrder,
  searchPurchaseOrder,
  createPurchaseOrderWithItem,
  getByPurchaseRequestId,
  updateStatusAndDocument,
  getPOStatistics,
};
