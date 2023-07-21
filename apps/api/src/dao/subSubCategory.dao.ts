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
    const subSubCategory = await transaction.sub_sub_category.create({
      data: {
        name,
        sub_category_id,
        budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
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
    const subSubCategory = await transaction.sub_sub_category.findUnique({
      where: {
        sub_sub_category_id: Number(subSubCategoryId),
      },
    });
    return subSubCategory;
  } catch (error) {
    console.log('Error occurred in subSubCategory getById dao', error);
    throw error;
  }
};

const getAll = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subSubCategories = await transaction.sub_sub_category.findMany({
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });

    const subCategoryIds = subSubCategories.map(
      (subSubCategory) => subSubCategory.sub_category_id
    );
    const subCategories = await transaction.sub_category.findMany({
      where: {
        sub_category_id: {
          in: subCategoryIds,
        },
      },
    });
    const subSubCategoriesWithSubCategory = subSubCategories.map(
      (subSubCategory) => {
        const subCategory = subCategories.find(
          (subCategory) =>
            subCategory.sub_category_id === subSubCategory.sub_category_id
        );
        return { ...subSubCategory, subCategory };
      }
    );

    return subSubCategoriesWithSubCategory;
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
    const subSubCategory = await transaction.sub_sub_category.delete({
      where: {
        sub_sub_category_id: Number(subSubCategoryId),
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

export default {
  add,
  edit,
  getById,
  getAll,
  deleteSubSubCategory,
};
