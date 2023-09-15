import indentRequestDetailsDao from '../dao/indentRequestDetails.dao';

/**
 * Method to search IndentRequestDetails - Pagination API
 * @returns
 */
const searchIndentRequestDetails = async (body) => {
  try {
    const offset = body.offset;
    const limit = body.limit;
    const order_by_column = body.order_by_column
      ? body.order_by_column
      : 'updated_by';
    const order_by_direction =
      body.order_by_direction === 'asc' ? 'asc' : 'desc';
    const global_search = body.global_search;
    const status = body.status;
    const indent_request_id = body.indent_request_id;

    const filterObj: any = {};

    if (status) {
      filterObj.filterIndentRequestDetails = {
        is_delete: status === 'AC' ? false : true,
      };
    }

    if (indent_request_id) {
      filterObj.filterIndentRequestDetails =
        filterObj.filterIndentRequestDetails || {};
      filterObj.filterIndentRequestDetails.AND =
        filterObj.filterIndentRequestDetails.AND || [];

      filterObj.filterIndentRequestDetails.AND.push({
        indent_request_id: indent_request_id,
      });
    }
    if (global_search) {
      filterObj.filterIndentRequestDetails =
        filterObj.filterIndentRequestDetails || {};
      filterObj.filterIndentRequestDetails.OR =
        filterObj.filterIndentRequestDetails.OR || [];

      filterObj.filterIndentRequestDetails.OR.push(
        {
          bom_detail_data: {
            bom_name: {
              contains: global_search,
              mode: 'insensitive',
            },
          },
        },

        {
          bom_detail_data: {
            uom_data: {
              name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          bom_detail_data: {
            sub_category_data: {
              name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          bom_detail_data: {
            item_data: {
              item_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          bom_detail_data: {
            machinery_data: {
              machinery_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        },
        {
          bom_detail_data: {
            labour_data: {
              labour_type: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        }
      );
    }

    const result = await indentRequestDetailsDao.searchIndentRequestDetails(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempIndentRequestData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempIndentRequestData;
  } catch (error) {
    console.log(
      'Error occurred in searchIndentRequestDetails IndentRequestDetails service : ',
      error
    );
    throw error;
  }
};

export { searchIndentRequestDetails };
