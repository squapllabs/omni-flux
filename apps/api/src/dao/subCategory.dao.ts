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
    const subCategory = await transaction.sub_category.create({
      data: {
        name,
        category_id,
        budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
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
    const subCategory = await transaction.sub_category.findUnique({
      where: {
        sub_category_id: Number(subCategoryId),
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
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
    });

    const categoryIds = subCategories.map(
      (subCategory) => subCategory.category_id
    );
    const categories = await transaction.category.findMany({
      where: {
        category_id: {
          in: categoryIds,
        },
      },
    });

    const subCategoriesWithCategory = subCategories.map((subCategory) => {
      const category = categories.find(
        (category) => category.category_id === subCategory.category_id
      );
      return { ...subCategory, category };
    });

    return subCategoriesWithCategory;
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
    const subCategory = await transaction.sub_category.delete({
      where: {
        sub_category_id: Number(subCategoryId),
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
      await transaction.$queryRaw`select * from sub_category sc where lower(sc."name") = lower(${subCategoryName}) and sc.category_id =${categoryId}`;
    return subCategory[0];
  } catch (error) {
    console.log(
      'Error occurred in subCategory getBySubCategoryNameAndCategoryId dao',
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
};
