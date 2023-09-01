import db from '../utils/db';

const add = async (
  bom_name: string,
  quantity: number,
  uom_id: number,
  category_id: number,
  sub_category_id: number,
  sub_sub_category_id: number,
  item_id: number,
  is_delete: boolean,
  created_by: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `
        INSERT INTO public."bom" (bom_name, quantity, uom_id, category_id, sub_category_id, sub_sub_category_id, item_id, is_delete, created_by,created_date,updated_date)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10,$11)
        RETURNING *;
      `;
    const result = await transaction.one(query, [
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
    console.error("Error occurred in BomDao add", error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `
        SELECT * FROM public."bom"`;
    const bom = await transaction.manyOrNone(query);
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
    const result = await transaction.oneOrNone(query, [bom_id]);
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
    const result = await transaction.one(query, [
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
    const result = await transaction.oneOrNone(query, [bom_id]);
    return result;
  } catch (error) {
    console.error('Error occure in bomDao deleteBom', error);
    throw error;
  }
};

const entireData = async (bom_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `SELECT
    b.*,
    JSON_BUILD_OBJECT(
        'uom_id', u.uom_id,
        'name', u.name,
        'description', u.description
    ) AS uom_data,
    JSON_BUILD_OBJECT(
        'category_id', c.category_id,
        'name', c.name,
        'project_id', c.project_id,
        'budget', c.budget,
        'description', c.description
    ) AS category_data,
    JSON_BUILD_OBJECT(
        'sub_category_id', sc.sub_category_id,
        'name', sc.name,
        'category_id', sc.category_id,
        'budget', sc.budget,
        'description', sc.description
    ) AS sub_category_data,
    JSON_BUILD_OBJECT(
        'sub_sub_category_id', ssc.sub_sub_category_id,
        'name', ssc.name,
        'sub_category_id', ssc.sub_category_id,
        'budget', ssc.budget,
        'description', ssc.description,
        'parent_sub_sub_category_id', ssc.parent_sub_sub_category_id
    ) AS sub_sub_category_data,
    JSON_BUILD_OBJECT(
        'item_id', i.item_id,
        'item_name', i.item_name,
        'sub_sub_category_id', i.sub_sub_category_id,
        'description', i.description,
        'hsn_code_id', i.hsn_code_id,
        'gst_id', i.gst_id,
        'uom_id', i.uom_id,
        'item_type_id', i.item_type_id,
        'brand_id', i.brand_id
    ) AS item_data
    FROM
        public.bom b
    left JOIN
        public.category c ON b.category_id = c.category_id
    left JOIN
        public.item i ON b.item_id = i.item_id
    left JOIN
        public.sub_category sc ON b.sub_category_id = sc.sub_category_id
    left JOIN
        public.sub_sub_category ssc ON b.sub_sub_category_id = ssc.sub_sub_category_id
    left JOIN
        public.uom u ON b.uom_id = u.uom_id
    WHERE
        b.bom_id = $1;
            `;

    const result = transaction.manyOrNone(query, [bom_id]);
    return result;
  } catch (error) {
    console.log("Error occure in bom.dao entireData ", error);
    throw (error);
  }
};

export default {
  add,
  getById,
  edit,
  deleteBom,
  getAll,
  entireData,
}
