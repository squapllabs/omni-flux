import hsnCodeDao from '../dao/hsnCode.dao';
import {
  createHsnCodeBody,
  updateHsnCodeBody,
} from '../interfaces/hsnCode.Interface';
import xlsx from 'xlsx';
import itemDao from '../dao/item.dao';

/**
 * Method to Create a New HsnCode
 * @param body
 * @returns
 */
const createHsnCode = async (body: createHsnCodeBody) => {
  try {
    const { code, description, created_by = null } = body;
    const hsnCodeDetails = await hsnCodeDao.add(code, description, created_by);
    const result = { success: true, data: hsnCodeDetails };
    return result;
  } catch (error) {
    console.log('Error occurred in hsnCode service Add: ', error);
    throw error;
  }
};

/**
 * Method to Update an Existing HsnCode
 * @param body
 * @returns
 */
const updateHsnCode = async (body: updateHsnCodeBody) => {
  try {
    const { code, description, updated_by, hsn_code_id } = body;
    let result = null;
    const hsnCodeExist = await hsnCodeDao.getById(hsn_code_id);
    if (hsnCodeExist) {
      const hsnCodeDetails = await hsnCodeDao.edit(
        code,
        description,
        updated_by,
        hsn_code_id
      );
      result = { success: true, data: hsnCodeDetails };
      return result;
    } else {
      result = { success: false, message: 'hsn_code_id not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in hsnCode service Edit: ', error);
    throw error;
  }
};

/**
 * Method to get HsnCode By HsnCodeId
 * @param hsnCodeId
 * @returns
 */
const getById = async (hsnCodeId: number) => {
  try {
    let result = null;
    const hsnCodeData = await hsnCodeDao.getById(hsnCodeId);
    if (hsnCodeData) {
      result = { success: true, data: hsnCodeData };
      return result;
    } else {
      result = { success: false, message: 'hsn_code_id does not exist' };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getById hsnCode service : ', error);
    throw error;
  }
};

/**
 * Method to Get All HsnCode's
 * @returns
 */
const getAllHsnCode = async () => {
  try {
    const result = await hsnCodeDao.getAll();
    const hsnCodeData = { success: true, data: result };
    return hsnCodeData;
  } catch (error) {
    console.log('Error occurred in getAllHsnCode hsnCode service : ', error);
    throw error;
  }
};

/**
 * Method to delete hsnCode
 * @param hsnCodeId
 */
const deleteHsnCode = async (hsnCodeId: number) => {
  try {
    const hsnCodeExist = await hsnCodeDao.getById(hsnCodeId);
    if (!hsnCodeExist) {
      const result = {
        message: 'HsnCode Id Not Exist',
        status: false,
        data: null,
      };
      return result;
    }
    const hsnCodeExistInItem = await itemDao.getByHSNCodeId(hsnCodeId);
    if (hsnCodeExistInItem) {
      const result = {
        message: 'Unable to delete.The hsn_code_id is mapped in item table',
        status: false,
        data: null,
      };
      return result;
    }
    const data = await hsnCodeDao.deleteHsnCode(hsnCodeId);
    if (data) {
      const result = {
        message: 'HsnCode Data Deleted Successfully',
        status: true,
        data: null,
      };
      return result;
    } else {
      const result = {
        status: false,
        message: 'Failed to delete this hsnCode',
        data: null,
      };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in deleteHsnCode hsnCode service : ', error);
    throw error;
  }
};

/**
 * Method to get HsnCode By code
 * @param code
 * @returns
 */
const getByCode = async (code: string) => {
  try {
    let result = null;
    const hsnCodeData = await hsnCodeDao.getByCode(code);
    if (hsnCodeData) {
      result = { success: true, is_exist: true, data: hsnCodeData };
      return result;
    } else {
      result = { success: false, is_exist: false };
      return result;
    }
  } catch (error) {
    console.log('Error occurred in getByCode hsnCode service : ', error);
    throw error;
  }
};

/**
 * Method to Add a bulk HSN Code By Excel Import
 * @param body
 * @returns
 */
const addBulkHSNCodeByImport = async (excelFile) => {
  try {
    const workbook = xlsx.read(excelFile.buffer, { type: 'buffer' });
    const sheet = workbook.Sheets[workbook.SheetNames[0]];
    const data: createHsnCodeBody[] = xlsx.utils.sheet_to_json(sheet);
    const transformedData = transformExcelData(data);
    const hsnCode = await hsnCodeDao.addBulk(transformedData);

    const result = {
      status: true,
      message: 'Success',
      data: hsnCode,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in hsnCode service addBulkHSNCode: ', error);
    throw error;
  }
};

/**
 * Method to convert the excel data into Array Of Object
 * @param data
 * @returns
 */
const transformExcelData = (
  data,
  created_by_from_request = null
): createHsnCodeBody[] => {
  const parsedData: createHsnCodeBody[] = data.map((hsnCode) => {
    let created_by =
      hsnCode.created_by === 'null' ||
      hsnCode.created_by === undefined ||
      hsnCode.created_by === null
        ? null
        : Number(hsnCode.created_by);

    created_by = created_by_from_request ? created_by_from_request : created_by;
    const currentDate = new Date();
    const code = String(hsnCode.code);
    return {
      code: code,
      description: hsnCode.description,
      created_by,
      created_date: currentDate,
      updated_date: currentDate,
    };
  });

  return parsedData;
};

/**
 * Method to Add a bulk HSN Code
 * @param body
 * @returns
 */
const addBulkHSNCode = async (body) => {
  try {
    const { created_by, items } = body;
    const convertedData = transformExcelData(items, created_by);
    const hsnCode = await hsnCodeDao.addBulk(convertedData);
    const result = {
      status: true,
      message: 'Success',
      data: hsnCode,
    };
    return result;
  } catch (error) {
    console.log('Error occurred in hsnCode service addBulkHSNCode: ', error);
    throw error;
  }
};

/**
 * Method to search HsnCode - Pagination API
 * @returns
 */
const searchHsnCode = async (body) => {
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
      filterHSNCode: {
        AND: [],
        OR: [
          { code: { contains: global_search, mode: 'insensitive' } },
          { description: { contains: global_search, mode: 'insensitive' } },
        ],
        is_delete: status === 'AC' ? false : true,
      },
    };

    const result = await hsnCodeDao.searchHSNCode(
      offset,
      limit,
      order_by_column,
      order_by_direction,
      filterObj
    );

    const count = result.count;
    const data = result.data;
    const total_pages = count < limit ? 1 : Math.ceil(count / limit);
    const tempHsnCodeData = {
      message: 'success',
      status: true,
      total_count: count,
      total_page: total_pages,
      content: data,
    };
    return tempHsnCodeData;
  } catch (error) {
    console.log('Error occurred in searchHsnCode HsnCode service : ', error);
    throw error;
  }
};

export {
  createHsnCode,
  updateHsnCode,
  getAllHsnCode,
  getById,
  deleteHsnCode,
  getByCode,
  addBulkHSNCodeByImport,
  addBulkHSNCode,
  searchHsnCode,
};
