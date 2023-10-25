import prisma from '../utils/prisma';
import common from './common/utils.dao';

const add = async (
  requester_user_id: number,
  requested_date: Date,
  request_status: string,
  priority: string,
  description: string,
  expected_delivery_date: Date,
  total_cost: number,
  approver_user_id: number,
  approver_status: string,
  approved_date: Date,
  rejected_date: Date,
  approver_comments: string,
  created_by: number,
  site_id: number,
  indent_request_details,
  project_id: number,
  request_type: string,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const formatted_requested_date = requested_date
      ? new Date(requested_date)
      : null;
    const formatted_expected_delivery_date = expected_delivery_date
      ? new Date(expected_delivery_date)
      : null;
    const formatted_approved_date = approved_date
      ? new Date(approved_date)
      : null;
    const formatted_rejected_date = rejected_date
      ? new Date(rejected_date)
      : null;

    const indentRequestCodeGeneratorQuery = `select concat('IND',DATE_PART('year', CURRENT_DATE),'00',nextval('indent_request_code_sequence')::text) as indent_request_code_sequence`;

    const indentRequestCode = await common.customQueryExecutor(
      indentRequestCodeGeneratorQuery
    );

    const transaction = connectionObj !== null ? connectionObj : prisma;

    const result = await transaction
      .$transaction(async (tx) => {
        const indentRequest = await tx.indent_request.create({
          data: {
            requester_user_id,
            requested_date: formatted_requested_date,
            request_status,
            priority,
            description,
            expected_delivery_date: formatted_expected_delivery_date,
            total_cost,
            approver_user_id,
            approver_status,
            approved_date: formatted_approved_date,
            rejected_date: formatted_rejected_date,
            approver_comments,
            project_id,
            created_by,
            site_id,
            created_date: currentDate,
            updated_date: currentDate,
            is_delete: is_delete,
            indent_request_code:
              indentRequestCode[0].indent_request_code_sequence,
            request_type,
          },
        });

        const new_indent_request_id = indentRequest?.indent_request_id;
        const indentRequestDetailsData = [];

        for (const indent_request_detail of indent_request_details) {
          const bom_detail_id = indent_request_detail.bom_detail_id;
          const indent_requested_quantity =
            indent_request_detail.indent_requested_quantity;
          const purchase_requested_quantity =
            indent_request_detail.purchase_requested_quantity
              ? indent_request_detail.purchase_requested_quantity
              : 0;
          const purchase_remaining_quantity = Math.max(
            0,
            indent_requested_quantity - purchase_requested_quantity
          );
          const total = indent_request_detail.total;
          const is_delete = indent_request_detail.is_delete;
          if (is_delete === false) {
            const indentRequestDetails = await tx.indent_request_details.create(
              {
                data: {
                  indent_request_id: new_indent_request_id,
                  bom_detail_id,
                  indent_requested_quantity,
                  purchase_requested_quantity,
                  purchase_remaining_quantity,
                  total,
                  created_by,
                  created_date: currentDate,
                  updated_date: currentDate,
                  is_delete: false,
                },
              }
            );
            indentRequestDetailsData.push(indentRequestDetails);
          }
        }

        const result = {
          indent_request: indentRequest,
          indent_request_details: indentRequestDetailsData,
        };

        return result;
      })
      .then((data) => {
        console.log('Successfully Indent Request Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in indentRequestDao add', error);
    throw error;
  }
};

const edit = async (
  requester_user_id: number,
  requested_date: Date,
  request_status: string,
  priority: string,
  description: string,
  expected_delivery_date: Date,
  total_cost: number,
  approver_user_id: number,
  approver_status: string,
  approved_date: Date,
  rejected_date: Date,
  approver_comments: string,
  updated_by: number,
  indent_request_details,
  project_id: number,
  site_id: number,
  indent_request_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_requested_date = requested_date
      ? new Date(requested_date)
      : null;
    const formatted_expected_delivery_date = expected_delivery_date
      ? new Date(expected_delivery_date)
      : null;
    const formatted_approved_date = approved_date
      ? new Date(approved_date)
      : null;
    const formatted_rejected_date = rejected_date
      ? new Date(rejected_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;

    const result = await transaction
      .$transaction(async (tx) => {
        const indentRequest = await tx.indent_request.update({
          where: {
            indent_request_id: indent_request_id,
          },
          data: {
            requester_user_id,
            requested_date: formatted_requested_date,
            request_status,
            priority,
            description,
            expected_delivery_date: formatted_expected_delivery_date,
            total_cost,
            approver_user_id,
            approver_status,
            approved_date: formatted_approved_date,
            rejected_date: formatted_rejected_date,
            approver_comments,
            project_id,
            site_id,
            updated_by,
            updated_date: currentDate,
          },
        });
        const indentRequestDetailsData = [];

        for (const indent_request_detail of indent_request_details) {
          const bom_detail_id = indent_request_detail.bom_detail_id;
          const indent_requested_quantity =
            indent_request_detail.indent_requested_quantity;
          const purchase_requested_quantity =
            indent_request_detail.purchase_requested_quantity
              ? indent_request_detail.purchase_requested_quantity
              : 0;
          const purchase_remaining_quantity = Math.max(
            0,
            indent_requested_quantity - purchase_requested_quantity
          );
          const total = indent_request_detail.total;
          const is_delete = indent_request_detail.is_delete;
          const indent_request_details_id =
            indent_request_detail.indent_request_details_id;

          if (indent_request_details_id) {
            if (is_delete === false) {
              const indentRequestDetails =
                await tx.indent_request_details.update({
                  where: {
                    indent_request_details_id: indent_request_details_id,
                  },
                  data: {
                    indent_request_id,
                    bom_detail_id,
                    indent_requested_quantity,
                    purchase_requested_quantity,
                    purchase_remaining_quantity,
                    total,
                    updated_by,
                    updated_date: currentDate,
                  },
                });
              indentRequestDetailsData.push(indentRequestDetails);
            } else if (is_delete === true) {
              await tx.indent_request_details.update({
                where: { indent_request_details_id: indent_request_details_id },
                data: {
                  is_delete: true,
                },
              });
            }
          } else if (is_delete === false) {
            const indentRequestDetails = await tx.indent_request_details.create(
              {
                data: {
                  indent_request_id,
                  bom_detail_id,
                  indent_requested_quantity,
                  purchase_requested_quantity,
                  purchase_remaining_quantity,
                  total,
                  created_by: updated_by,
                  created_date: currentDate,
                  updated_date: currentDate,
                  is_delete: false,
                },
              }
            );
            indentRequestDetailsData.push(indentRequestDetails);
          }
        }

        const result = {
          indent_request: indentRequest,
          indent_request_details: indentRequestDetailsData,
        };

        return result;
      })
      .then((data) => {
        console.log('Successfully Indent Request Data Returned ', data);
        return data;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in indentRequestDao edit', error);
    throw error;
  }
};

const getById = async (indentRequestId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const indentRequest = await transaction.indent_request.findFirst({
      where: {
        indent_request_id: Number(indentRequestId),
        is_delete: false,
      },
      include: {
        requester_user_data: { select: { first_name: true, last_name: true } },
        approver_user_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        site_data: true,
        indent_request_details: {
          where: { is_delete: false },
          include: {
            bom_detail_data: {
              include: {
                uom_data: { select: { name: true } },
                sub_category_data: { select: { name: true } },
                item_data: true,
                labour_data: true,
                machinery_data: true,
              },
            },
          },
          orderBy: [{ created_date: 'asc' }],
        },
      },
    });
    return indentRequest;
  } catch (error) {
    console.log('Error occurred in indentRequest getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const indentRequest = await transaction.indent_request.findMany({
      where: {
        is_delete: false,
      },
      include: {
        requester_user_data: { select: { first_name: true, last_name: true } },
        approver_user_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        site_data: true,
        indent_request_details: {
          where: { is_delete: false },
          include: {
            bom_detail_data: {
              include: {
                uom_data: { select: { name: true } },
                sub_category_data: { select: { name: true } },
                item_data: true,
                labour_data: true,
                machinery_data: true,
              },
            },
          },
          orderBy: [{ created_date: 'asc' }],
        },
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return indentRequest;
  } catch (error) {
    console.log('Error occurred in indentRequest getAll dao', error);
    throw error;
  }
};

const deleteIndentRequest = async (
  indentRequestId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const indentRequest = await transaction.indent_request.update({
      where: {
        indent_request_id: Number(indentRequestId),
      },
      data: {
        is_delete: true,
      },
    });
    return indentRequest;
  } catch (error) {
    console.log(
      'Error occurred in indentRequest deleteIndentRequest dao',
      error
    );
    throw error;
  }
};

const searchIndentRequest = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterIndentRequest;

    const indentRequestDataAvailablity =
      await transaction.indent_request.findMany({
        where: {
          is_delete: filter?.is_delete,
          project_id: filter?.AND[0]?.project_id,
        },
      });

    if (indentRequestDataAvailablity.length > 0) {
      const indentRequest = await transaction.indent_request.findMany({
        where: filter,
        include: {
          requester_user_data: {
            select: { first_name: true, last_name: true },
          },
          approver_user_data: { select: { first_name: true, last_name: true } },
          project_data: true,
          site_data: true,
          indent_request_details: {
            where: { is_delete: false },
            include: {
              bom_detail_data: {
                include: {
                  uom_data: { select: { name: true } },
                  sub_category_data: { select: { name: true } },
                  item_data: true,
                  labour_data: true,
                  machinery_data: true,
                },
              },
            },
            orderBy: [{ created_date: 'asc' }],
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
      const indentRequestCount = await transaction.indent_request.count({
        where: filter,
      });
      const indentRequestData = {
        count: indentRequestCount,
        data: indentRequest,
      };
      return indentRequestData;
    } else {
      return indentRequestDataAvailablity;
    }
  } catch (error) {
    console.log(
      'Error occurred in indentRequest dao : searchIndentRequest',
      error
    );
    throw error;
  }
};

const getByProjectId = async (project_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const indentRequest = await transaction.indent_request.findMany({
      where: {
        project_id: Number(project_id),
        is_delete: false,
      },
      include: {
        requester_user_data: { select: { first_name: true, last_name: true } },
        approver_user_data: { select: { first_name: true, last_name: true } },
        project_data: true,
        site_data: true,
        indent_request_details: {
          where: { is_delete: false },
          include: {
            bom_detail_data: {
              include: {
                uom_data: { select: { name: true } },
                sub_category_data: { select: { name: true } },
                item_data: true,
                labour_data: true,
                machinery_data: true,
              },
            },
          },
          orderBy: [{ created_date: 'asc' }],
        },
      },
    });
    return indentRequest;
  } catch (error) {
    console.log('Error occurred in indentRequest getByProjectId dao', error);
    throw error;
  }
};

const updateStatus = async (
  indent_request_id: number,
  approver_status: string,
  approver_comments: string,
  approved_date: Date,
  rejected_date: Date,
  updated_by: number,
  approver_user_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_approved_date = approved_date
      ? new Date(approved_date)
      : null;
    const formatted_rejected_date = rejected_date
      ? new Date(rejected_date)
      : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const indentRequest = await transaction.indent_request.update({
      where: {
        indent_request_id: Number(indent_request_id),
      },
      data: {
        approver_status,
        approver_comments,
        approved_date: formatted_approved_date,
        rejected_date: formatted_rejected_date,
        updated_by,
        approver_user_id,
        updated_date: currentDate,
      },
    });
    return indentRequest;
  } catch (error) {
    console.log('Error occurred in indentRequest updateStatus dao', error);
    throw error;
  }
};

export default {
  add,
  edit,
  getById,
  getAll,
  deleteIndentRequest,
  searchIndentRequest,
  getByProjectId,
  updateStatus,
};
