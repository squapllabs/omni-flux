import db from '../utils/db';
import prisma from '../utils/prisma';

const add = async (
  bom_name: string,
  quantity: number,
  uom_id: number,
  category_id: number,
  sub_category_id: number,
  sub_sub_category_id: number,
  item_id: number,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `
        INSERT INTO public."bom" (bom_name, quantity, uom_id, category_id, sub_category_id, sub_sub_category_id, item_id, is_delete, created_by,created_date,updated_date)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10,$11)
        RETURNING *;
      `;
    const result = await transaction.query(query, [
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      is_delete,
      created_by,
      currentDate,
      currentDate,
    ]);
    return result;
  } catch (error) {
    console.error('Error occurred in BomDao add', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `
        SELECT * FROM public."bom"`;
    const bom = await transaction.query(query);
    return bom;
  } catch (error) {
    console.log('Error occured in bomDao getAll', error);
    throw error;
  }
};

const getById = async (bom_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = 'SELECT * FROM public."bom" WHERE bom_id = $1';
    const result = await transaction.query(query, [bom_id]);
    return result;
  } catch (error) {
    console.error('Error occurred in BomDao getById:', error);
    throw error;
  }
};

const edit = async (
  bom_id: number,
  bom_name: string,
  quantity: number,
  uom_id: number,
  category_id: number,
  sub_category_id: number,
  sub_sub_category_id: number,
  item_id: number,
  is_delete: boolean,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `
        UPDATE public."bom"
        SET bom_name = $1, quantity = $2, uom_id = $3, category_id = $4,
            sub_category_id = $5, sub_sub_category_id = $6, item_id = $7,
            is_delete = $8, updated_by = $9,updated_date = $11
        WHERE bom_id = $10
        RETURNING *;
      `;
    const result = await transaction.query(query, [
      bom_name,
      quantity,
      uom_id,
      category_id,
      sub_category_id,
      sub_sub_category_id,
      item_id,
      is_delete,
      updated_by,
      bom_id,
      currentDate,
    ]);
    return result;
  } catch (error) {
    console.error('Error occurred in BomDao edit', error);
    throw error;
  }
};

const deleteBom = async (bom_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `update public."bom" set is_delete = true WHERE bom_id = $1 RETURNING *`;
    const result = await transaction.query(query, [bom_id]);
    return result;
  } catch (error) {
    console.error('Error occure in bomDao deleteBom', error);
    throw error;
  }
};

const getByCategorySubCatAndSubSubCatId = async (
  categoryId: number,
  subCategoryId: number,
  subSubCategoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    let result = null;
    if (categoryId && subCategoryId && subSubCategoryId) {
      const bom = await transaction.bom.findMany({
        where: {
          category_id: Number(categoryId),
          sub_category_id: Number(subCategoryId),
          sub_sub_category_id: Number(subSubCategoryId),
          is_delete: false,
        },
        include: {
          uom_data: true,
          category_data: true,
          sub_category_data: true,
          sub_sub_category_data: true,
          item_data: true,
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
      const count = await transaction.bom.count({
        where: {
          category_id: Number(categoryId),
          sub_category_id: Number(subCategoryId),
          sub_sub_category_id: Number(subSubCategoryId),
          is_delete: false,
        },
      });
      result = { count: count, data: bom };
      return result;
    } else if (categoryId && subCategoryId) {
      const bom = await transaction.bom.findMany({
        where: {
          category_id: Number(categoryId),
          sub_category_id: Number(subCategoryId),
          is_delete: false,
        },
        include: {
          uom_data: true,
          category_data: true,
          sub_category_data: true,
          sub_sub_category_data: true,
          item_data: true,
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
      const count = await transaction.bom.count({
        where: {
          category_id: Number(categoryId),
          sub_category_id: Number(subCategoryId),
          is_delete: false,
        },
      });
      result = { count: count, data: bom };
      return result;
    } else if (categoryId) {
      const bom = await transaction.bom.findMany({
        where: {
          category_id: Number(categoryId),
          is_delete: false,
        },
        include: {
          uom_data: true,
          category_data: true,
          sub_category_data: true,
          sub_sub_category_data: true,
          item_data: true,
        },
        orderBy: [
          {
            updated_date: 'desc',
          },
        ],
      });
      const count = await transaction.bom.count({
        where: {
          category_id: Number(categoryId),
          is_delete: false,
        },
      });
      result = { count: count, data: bom };
      return result;
    }
  } catch (error) {
    console.log(
      'Error occurred in bom dao : getByCategorySubCatAndSubSubCatId',
      error
    );
    throw error;
  }
};

const getByUomId = async (uomId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bom = await transaction.bom.findFirst({
      where: {
        uom_id: Number(uomId),
      },
    });
    return bom;
  } catch (error) {
    console.log('Error occurred in bom dao : getByUomId', error);
    throw error;
  }
};

export default {
  add,
  getById,
  edit,
  deleteBom,
  getAll,
  getByCategorySubCatAndSubSubCatId,
  getByUomId,
};
