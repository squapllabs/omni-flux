import indentRequestDao from '../dao/indentRequest.dao';
import itemDao from '../dao/item.dao';
import { createItemBody, updateItemBody } from '../interfaces/item.interface';
import prisma from '../utils/prisma';
import xlsx from 'xlsx';
/**
 * Method to Add a new item
 * @param body
 * @returns
 */
const addItem = async (body: createItemBody) => {
  let result = null;
  try {
    const brand_id = body.brand_id;
    const item_name = body.item_name;
    const description = body.description;
    const hsn_code_id = body.hsn_code_id;
    const gst_id = body.gst_id;
    const uom_id = body.uom_id;
    const created_by = body.created_by;
    const updated_by = body.updated_by;
    const item_type_id = body.item_type_id;
    const rate = body.rate;

    result = await prisma
      .$transaction(async (prisma) => {
        const CreateItem = await itemDao.add(
          item_name,
          description,
          hsn_code_id,
          gst_id,
          uom_id,
          created_by,
          updated_by,
          item_type_id,
          brand_id,
          rate,
          prisma
        );

        return CreateItem;
      })
      .then((data) => {
        console.log('Successfully item Data Returned ', data);
        const newItemData = {
          message: 'success',
          status: true,
          data: data,
        };
        return newItemData;
      })
      .catch((error: string) => {
        console.log('Failure, ROLLBACK was executed', error);
        throw error;
      });
    return result;
  } catch (error) {
    console.log('Error occurred in item service: ', error);
    throw error;
  }
};
/**
 * Method to Add a bulk item
 * @param body
 * @returns
 */

const addBulkItems = async (req) => {
  try {
    const workbook = xlsx.read(req.file.buffer, { type: 'buffer' });
    const sheet = workbook.Sheets[workbook.SheetNames[0]];
    const data: createItemBody[] = xlsx.utils.sheet_to_json(sheet);
    const transformedData = transformExcelData(data);
    const result = await itemDao.addBulk(transformedData);

    console.log('Successfully/ inserted bulk item', result);

    return {
      message: 'success',
      status: true,
      data: result,
    };
  } catch (error) {
    console.log('Error occurred in item service Add: ', error);
    throw error;
  }
};
const transformExcelData = (data: any[]): createItemBody[] => {
  const parsedData: createItemBody[] = data.map((item) => {
    const created_by =
      item.created_by === 'null' ? null : BigInt(item.created_by);
    const updated_by =
      item.updated_by === 'null' ? null : BigInt(item.updated_by);
    const currentDate = new Date();
    return {
      item_name: item.item_name,
      description: item.description,
      hsn_code_id: Number(item.hsn_code_id),
      gst_id: Number(item.gst_id),
      uom_id: Number(item.uom_id),
      created_by,
      updated_by,
      created_date: currentDate,
      updated_date: currentDate,
      item_type_id: Number(item.item_type_id),
      brand_id: Number(item.brand_id),
      is_delete: false,
      rate: Number(item.rate),
    };
  });

  return parsedData;
};
/**
 * Method to Get All item
 * @returns
 */

const getAllItem = async (data) => {
  try {
    const offset = data.offset;
    const limit = data.limit;
    const default_column = 'item_name';
    const order_by_column = data.order_by_column || default_column;
    const order_by_direction =
      data.order_by_direction === 'asc' ? 'asc' : 'desc';
    const offsetValue = parseInt(offset, 10) || 0;
    const limitValue = parseInt(limit, 10) || 50;
    const filters = data.filters;
    const filterObj = {
      filterItem: {
        AND: [],
      },
    };

    if (filters) {
      for (const filter of filters) {
        const field_name = filter.field_name;
        const operator = filter.operator;
        const field_value = filter.field_value;

        await applyFilter(filterObj, field_name, operator, field_value);
      }
    }
    const result = await itemDao.getAll(
      offsetValue,
      limitValue,
      order_by_column,
      order_by_direction,
      filterObj
    );
    const total_page = Math.round(result.totalCount / limitValue);
    const itemData = {
      message: 'success',
      status: true,
      total_count: result.totalCount,
      total_page,
      data: result.items,
    };
    return itemData;
  } catch (error) {
    console.log('Error occurred in getAll item service: ', error);
    throw error;
  }
};
const applyFilter = async (filterObj, field_name, operator, field_value) => {
  if (operator === 'Equal') {
    filterObj.filterItem[field_name] = field_value;
  } else if (operator === 'Not Equal') {
    filterObj.filterItem[field_name] = { NOT: { equals: field_value } };
  } else if (operator === 'Like') {
    filterObj.filterItem[field_name] = { contains: field_value };
  } else if (operator === 'Not Like') {
    filterObj.filterItem[field_name] = { NOT: { contains: field_value } };
  } else if (operator === 'In') {
    filterObj.filterItem[field_name] = { in: field_value };
  } else if (operator === 'Not In') {
    filterObj.filterItem[field_name] = { NOT: { in: field_value } };
  } else if (operator === 'Is') {
    filterObj.filterItem[field_name] = null;
  } else {
    throw new Error(`Unsupported operator: ${operator}`);
  }
};

/**
 * Method to Get All Item By search
 * @returns
 */
const getAllItemBySearch = async (data) => {
  try {
    const keyword = data.keyword;
    let result = null;
    const itemData = await itemDao.getAllBySearch(keyword);
    if (itemData) {
      result = { message: 'success', status: true, data: itemData };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getAll item service: ', error);
    throw error;
  }
};

/**
 * Method to get item By itemId
 * @param ItemId
 * @returns
 */
const getById = async (item_id: number) => {
  try {
    let result = null;
    const itemData = await itemDao.getById(item_id);
    if (itemData) {
      result = { message: 'success', status: true, data: itemData };
      return result;
    } else {
      result = { message: 'item Id does not exist', status: false, data: null };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById item service : ', error);
    throw error;
  }
};

/**
 * Method to delete item
 * @param itemId
 */
const deleteItem = async (item_id: number) => {
  try {
    const itemExist = await itemDao.getById(item_id);
    if (!itemExist) {
      const result = {
        message: 'item_id does not exist',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await itemDao.deleteItem(item_id);
    if (data) {
      const result = {
        message: 'Successfully deleted this item',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        message: 'Failed to delete this item',
        status: false,
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteItem item service : ', error);
    throw error;
  }
};
/**
 * Method to Update an Existing item
 * @param body
 * @returns
 */
const updateItem = async (body: updateItemBody) => {
  try {
    const item_id = body.item_id;
    const item_name = body.item_name;
    const description = body.description;
    const hsn_code_id = body.hsn_code_id;
    const gst_id = body.gst_id;
    const uom_id = body.uom_id;
    const updated_by = body.updated_by;
    const item_type_id = body.item_type_id;
    const brand_id = body.brand_id;
    const rate = body.rate;
    let result = null;
    const ItemExist = await itemDao.getById(item_id);
    if (ItemExist) {
      const itemDetails = await itemDao.edit(
        item_id,
        item_name,
        description,
        hsn_code_id,
        gst_id,
        uom_id,
        updated_by,
        item_type_id,
        brand_id,
        rate
      );
      result = { message: 'success', status: true, data: itemDetails };
      return result;
    } else {
      result = { message: 'item_id does not exist', status: false, data: null };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in item service Edit: ', error);
    throw error;
  }
};

/**
 * Method to getAll Items
 * @returns
 */
const getAllItemData = async () => {
  try {
    let result = null;
    const itemData = await itemDao.getAllItems();
    if (itemData) {
      result = { message: 'success', status: true, data: itemData };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getAllItemData item service : ', error);
    throw error;
  }
};

/**
 * Method to search Item - Pagination API
 * @returns
 */
const searchItem = async (body) => {
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
    const filterObj = {
      filterItem: {
        AND: [],
        OR: [
          { item_name: { contains: global_search, mode: 'insensitive' } },
          { description: { contains: global_search, mode: 'insensitive' } },
          {
            hsn_code: {
              code: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
          {
            brand: {
              brand_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
          {
            item_type: {
              master_data_name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
          {
            uom: {
              name: {
                contains: global_search,
                mode: 'insensitive',
              },
            },
          },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await itemDao.searchItem(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );
    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    if (result.count > 0) {
      const tempItemData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempItemData;
    } else if (result.count === 0) {
      const tempItemData = {
        message: 'success',
        status: true,
        total_count: count,
        total_page: total_pages,
        is_available: true,
        content: data,
      };
      return tempItemData;
    } else {
      const tempItemData = {
        message: 'fail',
        status: true,
        is_available: false,
      };
      return tempItemData;
    }
  } catch (error) {
    console.log('Error occurred in searchItem Item service : ', error);
    throw error;
  }
};

/**
 * Method to get item by item name
 * @param item_name
 * @returns {Promise<object>} Result object
 */
const getByItemName = async (item_name) => {
  try {
    const itemData = await itemDao.getByItemName(item_name);
    const isExist = !!itemData;
    return {
      message: isExist ? 'item_name already exists' : 'item_name is available',
      status: isExist,
      is_exist: isExist,
      data: itemData,
    };
  } catch (error) {
    console.log('Error occurred in getByItemName item service: ', error);
    throw error;
  }
};

/**
 * Method to get item By Indent Request Id
 * @param indent_request_id
 * @returns
 */
const getByIndentRequestId = async (indent_request_id: number) => {
  try {
    const indentRequestExist = await indentRequestDao.getById(
      indent_request_id
    );
    if (!indentRequestExist) {
      return {
        message: 'indent_request_id does not exist',
        status: false,
        data: null,
      };
    }

    const itemData = await itemDao.getByIndentRequestId(indent_request_id);
    if (itemData.length > 0) {
      return { message: 'success', status: true, data: itemData };
    } else {
      return {
        message: 'No data found for this indent_request_id',
        status: false,
        data: null,
      };
    }
  } catch (error) {
    console.log(
      'Error occurred in getByIndentRequestId item service : ',
      error
    );
    throw error;
  }
};

export {
  addItem,
  getAllItem,
  getById,
  deleteItem,
  updateItem,
  addBulkItems,
  getAllItemBySearch,
  getAllItemData,
  searchItem,
  getByItemName,
  getByIndentRequestId,
};
