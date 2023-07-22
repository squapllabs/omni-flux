import prisma from '../utils/prisma';

const add = async (
  name: string,
  sub_category_id: number,
  budget: number,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const subSubCategory = await transaction.sub_sub_category.create({
      data: {
        name,
        sub_category_id,
        budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return subSubCategory;
  } catch (error) {
    console.log('Error occurred in subSubCategoryDao add', error);
    throw error;
  }
};

const edit = async (
  name: string,
  sub_category_id: number,
  budget: number,
  updated_by: bigint,
  sub_sub_category_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategory = await transaction.sub_sub_category.update({
      where: {
        sub_sub_category_id: sub_sub_category_id,
      },
      data: {
        name,
        sub_category_id,
        budget,
        updated_by,
        updated_date: currentDate,
      },
    });
    return subSubCategory;
  } catch (error) {
    console.log('Error occurred in subSubCategoryDao edit', error);
    throw error;
  }
};

const getById = async (subSubCategoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategory = await transaction.sub_sub_category.findFirst({
      where: {
        sub_sub_category_id: Number(subSubCategoryId),
        is_delete: false,
      },
      include: {
        sub_category: true,
      },
    });
    return subSubCategory;
  } catch (error) {
    console.log('Error occurred in subSubCategory getById dao', error);
    throw error;
  }
};

/* const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategories = await transaction.sub_sub_category.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
      include: {
        sub_category: true,
      },
    });

    return subSubCategories;
  } catch (error) {
    console.log('Error occurred in subSubCategory getAll dao', error);
    throw error;
  }
}; */

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategories = await transaction.$queryRaw`select
      jsonb_build_object('sub_sub_category_id', ssc.sub_sub_category_id , 'sub_sub_category_name', ssc."name", 'budget', ssc.budget, 'created_date', ssc.created_date, 'updated_date', ssc.updated_date)as sub_sub_category_details,
      jsonb_build_object('sub_category_id', sc.sub_category_id, 'sub_category_name', sc."name", 'budget', sc.budget)as sub_category_details,
      jsonb_build_object('category_id', c.category_id, 'category_name', c."name", 'budget', c.budget)as category_details
    from
      sub_sub_category ssc
    left join sub_category sc on
      sc.sub_category_id = ssc.sub_category_id
    left join category c on
      c.category_id = sc.category_id
    order by
      ssc.updated_date desc`;
    return subSubCategories;
  } catch (error) {
    console.log('Error occurred in subSubCategory getAll dao', error);
    throw error;
  }
};

const deleteSubSubCategory = async (
  subSubCategoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategory = await transaction.sub_sub_category.update({
      where: {
        sub_sub_category_id: Number(subSubCategoryId),
      },
      data: {
        is_delete: true,
      },
    });
    return subSubCategory;
  } catch (error) {
    console.log(
      'Error occurred in subSubCategory deleteSubSubCategory dao',
      error
    );
    throw error;
  }
};

const getBySubSubCategoryNameAndSubCategoryId = async (
  subSubCategoryName: string,
  subCategoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategory =
      await transaction.$queryRaw`select * from sub_sub_category ssc where lower(ssc."name") = lower(${subSubCategoryName}) and ssc.sub_category_id =${subCategoryId} and ssc.is_delete=false`;
    return subSubCategory[0];
  } catch (error) {
    console.log(
      'Error occurred in subSubCategory getBySubSubCategoryNameAndSubCategoryId dao',
      error
    );
    throw error;
  }
};

const getAllInActiveSubSubCategories = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategory = await transaction.sub_sub_category.findMany({
      where: {
        is_delete: true,
      },
    });
    return subSubCategory;
  } catch (error) {
    console.log(
      'Error occurred in subSubCategory getAllInActiveSubSubCategories dao',
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
  deleteSubSubCategory,
  getBySubSubCategoryNameAndSubCategoryId,
  getAllInActiveSubSubCategories,
};
