import prisma from '../utils/prisma';

const add = async (
  name: string,
  sub_category_id: number,
  budget: number,
  created_by: bigint,
  description: string,
  project_id: number,
  parent_sub_sub_category_id: number,
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
        description,
        project_id,
        parent_sub_sub_category_id,
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
  description: string,
  project_id: number,
  parent_sub_sub_category_id: number,
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
        description,
        project_id,
        parent_sub_sub_category_id,
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
        parent_data: true,
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
      where: {
        is_delete: false,
      },
      orderBy: [
        {
          updated_date: 'desc',
        },
      ],
      include: {
        sub_category: {
          include: {
            category: true,
          },
        },
        parent_data: true,
      },
    });

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
    const currentDate = new Date();
    const subSubCategory = await transaction.sub_sub_category.update({
      where: {
        sub_sub_category_id: Number(subSubCategoryId),
      },
      data: {
        is_delete: true,
        updated_date: currentDate,
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

const searchSubSubCategory = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterSubSubCategory;
    const subSubCategory = await transaction.sub_sub_category.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      include: {
        sub_category: {
          include: {
            category: true,
          },
        },
        parent_data: true,
      },
      skip: offset,
      take: limit,
    });
    const subSubCategoryCount = await transaction.sub_sub_category.count({
      where: filter,
    });
    const subSubCategoryData = {
      count: subSubCategoryCount,
      data: subSubCategory,
    };
    return subSubCategoryData;
  } catch (error) {
    console.log(
      'Error occurred in subSubCategory dao : searchSubSubCategory ',
      error
    );
    throw error;
  }
};

const getBySubCategoryId = async (
  subCategoryId: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_sub_category.findMany({
      where: {
        sub_category_id: Number(subCategoryId),
        is_delete: false,
      },
      include: {
        sub_category: {
          include: {
            category: true,
          },
        },
        parent_data: true,
      },
    });
    return subCategory;
  } catch (error) {
    console.log(
      'Error occurred in subSubCategory getBySubCategoryId dao',
      error
    );
    throw error;
  }
};

const getAllParentData = async (connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_sub_category.findMany({
      where: {
        parent_sub_sub_category_id: null,
        is_delete: false,
      },
      include: {
        sub_category: {
          include: {
            category: true,
          },
        },
        parent_data: true,
        child_data: true,
      },
    });
    return subCategory;
  } catch (error) {
    console.log('Error occurred in subSubCategory getAllParentData dao', error);
    throw error;
  }
};

const getChildDataByParentSubSubCatId = async (
  parent_sub_sub_category_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_sub_category.findMany({
      where: {
        parent_sub_sub_category_id: Number(parent_sub_sub_category_id),
        is_delete: false,
      },
      include: {
        sub_category: {
          include: {
            category: true,
          },
        },
        parent_data: true,
        child_data: true,
      },
    });
    return subCategory;
  } catch (error) {
    console.log(
      'Error occurred in subSubCategory getChildDataByParentSubSubCatId dao',
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
  searchSubSubCategory,
  getBySubCategoryId,
  getAllParentData,
  getChildDataByParentSubSubCatId,
};
