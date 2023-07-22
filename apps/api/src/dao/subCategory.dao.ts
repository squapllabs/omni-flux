import prisma from '../utils/prisma';

const add = async (
  name: string,
  category_id: number,
  budget: number,
  created_by: bigint,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const subCategory = await transaction.sub_category.create({
      data: {
        name,
        category_id,
        budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
      },
    });
    return subCategory;
  } catch (error) {
    console.log('Error occurred in subCategoryDao add', error);
    throw error;
  }
};

const edit = async (
  name: string,
  category_id: number,
  budget: number,
  updated_by: bigint,
  sub_category_id: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.update({
      where: {
        sub_category_id: sub_category_id,
      },
      data: {
        name,
        category_id,
        budget,
        updated_by,
        updated_date: currentDate,
      },
    });
    return subCategory;
  } catch (error) {
    console.log('Error occurred in subCategoryDao edit', error);
    throw error;
  }
};

const getById = async (subCategoryId: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.findFirst({
      where: {
        sub_category_id: Number(subCategoryId),
        is_delete: false,
      },
      include: {
        category: true,
      },
    });
    return subCategory;
  } catch (error) {
    console.log('Error occurred in subCategory getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategories = await transaction.sub_category.findMany({
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
      include: {
        category: true,
      },
    });
    return subCategories;
  } catch (error) {
    console.log('Error occurred in subCategory getAll dao', error);
    throw error;
  }
};

const deleteSubCategory = async (
  subCategoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.update({
      where: {
        sub_category_id: Number(subCategoryId),
      },
      data: {
        is_delete: true,
      },
    });
    return subCategory;
  } catch (error) {
    console.log('Error occurred in subCategory deleteSubCategory dao', error);
    throw error;
  }
};

const getBySubCategoryNameAndCategoryId = async (
  subCategoryName: string,
  categoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory =
      await transaction.$queryRaw`select * from sub_category sc where lower(sc."name") = lower(${subCategoryName}) and sc.category_id =${categoryId} and sc.is_delete=false`;
    return subCategory[0];
  } catch (error) {
    console.log(
      'Error occurred in subCategory getBySubCategoryNameAndCategoryId dao',
      error
    );
    throw error;
  }
};

const getAllInActiveSubCategories = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const SubCategory = await transaction.sub_category.findMany({
      where: {
        is_delete: true,
      },
    });
    return SubCategory;
  } catch (error) {
    console.log(
      'Error occurred in subCategory getAllInActiveSubCategories dao',
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
  deleteSubCategory,
  getBySubCategoryNameAndCategoryId,
  getAllInActiveSubCategories,
};
