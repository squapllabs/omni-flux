import { bomBody } from '../interfaces/bom.interface';
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
  description: string,
  rate: number,
  total: number,
  bom_type: string,
  machinery_id: number,
  labour_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const is_delete = false;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bom = transaction.bom.create({
      data: {
        bom_name,
        quantity,
        uom_id,
        category_id,
        sub_category_id,
        sub_sub_category_id,
        item_id,
        created_by,
        description,
        rate,
        total,
        bom_type,
        machinery_id,
        labour_id,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return bom;
  } catch (error) {
    console.error('Error occurred in BomDao add', error);
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
  updated_by: number,
  description: string,
  rate: number,
  total: number,
  bom_type: string,
  machinery_id: number,
  labour_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bom = transaction.bom.update({
      where: { bom_id: bom_id },
      data: {
        bom_name,
        quantity,
        uom_id,
        category_id,
        sub_category_id,
        sub_sub_category_id,
        item_id,
        description,
        rate,
        total,
        bom_type,
        machinery_id,
        labour_id,
        updated_date: currentDate,
        updated_by,
      },
    });
    return bom;
  } catch (error) {
    console.error('Error occurred in BomDao edit', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bom = await transaction.bom.findMany({
      where: {
        is_delete: false,
      },
      include: {
        uom_data: true,
        category_data: true,
        sub_category_data: true,
        sub_sub_category_data: true,
        item_data: true,
        labour_data: true,
        machinery_data: true,
      },
      orderBy: [{ updated_date: 'desc' }],
    });
    return bom;
  } catch (error) {
    console.log('Error occured in bomDao getAll', error);
    throw error;
  }
};

const getById = async (bom_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bom = await transaction.bom.findFirst({
      where: {
        bom_id: Number(bom_id),
        is_delete: false,
      },
      include: {
        uom_data: true,
        category_data: true,
        sub_category_data: true,
        sub_sub_category_data: true,
        item_data: true,
        labour_data: true,
        machinery_data: true,
      },
    });
    return bom;
  } catch (error) {
    console.error('Error occurred in BomDao getById:', error);
    throw error;
  }
};

const deleteBom = async (bom_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bom = await transaction.bom.update({
      where: { bom_id: Number(bom_id) },
      data: { is_delete: true },
    });
    return bom;
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
          labour_data: true,
          machinery_data: true,
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
          labour_data: true,
          machinery_data: true,
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
          labour_data: true,
          machinery_data: true,
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
    console.log('Error occure in bom.dao entireData ', error);
    throw error;
  }
};

const addBulk = async (bulkBom: bomBody[], connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bomData = [];
    const currentDate = new Date();
    for (const bom of bulkBom) {
      const created_by = bom.created_by ? bom.created_by : null;
      const updated_by = bom.updated_by ? bom.updated_by : null;
      const is_delete = bom.is_delete;
      const bom_name = bom.bom_name;
      const item_id = bom.item_id;
      const uom_id = bom.uom_id;
      const description = bom.description;
      const quantity = bom.quantity;
      const rate = bom.rate;
      const total = bom.total;
      const sub_category_id = bom.sub_category_id;
      const bom_type = bom.bom_type;
      const category_id = bom.category_id;
      const sub_sub_category_id = bom.sub_sub_category_id;
      const bom_id = bom.bom_id;
      const machinery_id = bom.machinery_id;
      const labour_id = bom.labour_id;
      if (bom_id) {
        if (is_delete === true) {
          await transaction.bom.update({
            where: { bom_id: bom_id },
            data: {
              is_delete: true,
            },
          });
        } else {
          const bomResult = await transaction.bom.update({
            where: { bom_id: bom_id },
            data: {
              bom_name,
              quantity,
              uom_id,
              category_id,
              sub_category_id,
              sub_sub_category_id,
              item_id,
              updated_by,
              description,
              rate,
              total,
              bom_type,
              machinery_id,
              labour_id,
              updated_date: currentDate,
            },
          });
          bomData.push(bomResult);
        }
      } else if (is_delete === false) {
        const bomResult = await transaction.bom.create({
          data: {
            bom_name,
            quantity,
            uom_id,
            category_id,
            sub_category_id,
            sub_sub_category_id,
            item_id,
            created_by,
            description,
            rate,
            total,
            bom_type,
            machinery_id,
            labour_id,
            created_date: currentDate,
            updated_date: currentDate,
            is_delete: false,
          },
        });
        bomData.push(bomResult);
      }
    }
    return bomData;
  } catch (error) {
    console.log('Error occurred in bomDao addBulk ', error);
    throw error;
  }
};

const getBomBySubCategoryIdAndBomType = async (
  sub_category_id: number,
  bom_type: string,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const bom = await transaction.bom.findMany({
      where: {
        is_delete: false,
        sub_category_id: Number(sub_category_id),
        bom_type: bom_type,
      },
      include: {
        uom_data: true,
        category_data: true,
        sub_category_data: true,
        sub_sub_category_data: true,
        item_data: true,
        labour_data: true,
        machinery_data: true,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });
    return bom;
  } catch (error) {
    console.log(
      'Error occurred in bomDao getBomBySubCategoryIdAndBomType ',
      error
    );
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
  entireData,
  addBulk,
  getBomBySubCategoryIdAndBomType,
};
