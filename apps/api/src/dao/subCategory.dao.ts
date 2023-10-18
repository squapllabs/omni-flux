import db from '../utils/db';
import prisma from '../utils/prisma';

const add = async (
  name: string,
  category_id: number,
  actual_budget: number,
  created_by: bigint,
  description: string,
  project_id: number,
  start_date: Date,
  end_date: Date,
  progress_status: string,
  bom_configuration_id: number,
  parent_sub_category_id: number,
  estimated_budget: number,
  uom_id: number,
  quantity: number,
  rate: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const is_delete = false;
    const subCategory = await transaction.sub_category.create({
      data: {
        name,
        category_id,
        actual_budget,
        created_by,
        created_date: currentDate,
        updated_date: currentDate,
        is_delete: is_delete,
        description,
        project_id,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        bom_configuration_id,
        progress_status,
        parent_sub_category_id,
        estimated_budget,
        uom_id,
        quantity,
        rate,
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
  actual_budget: number,
  updated_by: bigint,
  sub_category_id: number,
  description: string,
  project_id: number,
  start_date: Date,
  end_date: Date,
  progress_status: string,
  bom_configuration_id: number,
  parent_sub_category_id: number,
  estimated_budget: number,
  uom_id: number,
  quantity: number,
  rate: number,
  connectionObj = null
) => {
  try {
    const currentDate = new Date();
    const formatted_start_date = start_date ? new Date(start_date) : null;
    const formatted_end_date = end_date ? new Date(end_date) : null;
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.update({
      where: {
        sub_category_id: sub_category_id,
      },
      data: {
        name,
        category_id,
        actual_budget,
        updated_by,
        updated_date: currentDate,
        description,
        project_id,
        start_date: formatted_start_date,
        end_date: formatted_end_date,
        bom_configuration_id,
        progress_status,
        parent_sub_category_id,
        estimated_budget,
        uom_id,
        quantity,
        rate,
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
        project_data: {
          select: {
            project_name: true,
            description: true,
          },
        },
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
        uom_data: {
          select: { name: true },
        },
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
      orderBy: [{ created_date: 'asc' }],
      include: {
        category: true,
        project_data: {
          select: {
            project_name: true,
            description: true,
          },
        },
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
        uom_data: {
          select: { name: true },
        },
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
    const currentDate = new Date();
    const subCategory = await transaction.sub_category.update({
      where: {
        sub_category_id: Number(subCategoryId),
      },
      data: {
        is_delete: true,
        updated_date: currentDate,
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
    const subCategory = await transaction.sub_category.findMany({
      where: {
        is_delete: true,
      },
      orderBy: [{ created_date: 'asc' }],
    });
    return subCategory;
  } catch (error) {
    console.log(
      'Error occurred in subCategory getAllInActiveSubCategories dao',
      error
    );
    throw error;
  }
};

const searchSubCategory = async (
  offset: number,
  limit: number,
  orderByColumn: string,
  orderByDirection: string,
  filters,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const filter = filters.filterSubCategory;
    const subCategory = await transaction.sub_category.findMany({
      where: filter,
      orderBy: [
        {
          [orderByColumn]: orderByDirection,
        },
      ],
      include: {
        category: true,
        project_data: {
          select: {
            project_name: true,
            description: true,
          },
        },
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
        uom_data: {
          select: { name: true },
        },
      },
      skip: offset,
      take: limit,
    });
    const subCategoryCount = await transaction.sub_category.count({
      where: filter,
    });
    const subCategoryData = {
      count: subCategoryCount,
      data: subCategory,
    };
    return subCategoryData;
  } catch (error) {
    console.log(
      'Error occurred in subCategory dao : searchSubCategory ',
      error
    );
    throw error;
  }
};

const getByCategoryId = async (category_id: number, connectionObj = null) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.findMany({
      where: {
        category_id: Number(category_id),
        is_delete: false,
      },
      include: {
        category: true,
        project_data: {
          select: {
            project_name: true,
            description: true,
          },
        },
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
        uom_data: {
          select: { name: true },
        },
      },
      orderBy: [{ created_date: 'asc' }],
    });
    return subCategory;
  } catch (error) {
    console.log('Error occurred in subCategory getByCategoryId dao', error);
    throw error;
  }
};

const getByCategoryIdAndBomConfigurationId = async (
  category_id: number,
  bom_configuration_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.findMany({
      where: {
        category_id: Number(category_id),
        bom_configuration_id: Number(bom_configuration_id),
        is_delete: false,
        parent_sub_category_id: null,
      },
      include: {
        children: true,
        category: true,
        project_data: {
          select: {
            project_name: true,
            description: true,
          },
        },
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
        bom_detail: {
          where: {
            bom_configuration_id: Number(bom_configuration_id),
            is_delete: false,
          },
        },
        uom_data: {
          select: { name: true },
        },
      },
      orderBy: [{ created_date: 'asc' }],
    });

    const response = subCategory.map((bomDetail) => ({
      ...bomDetail,
      is_bom_detail: bomDetail.bom_detail.length > 0 ? true : false,
    }));

    return response;
  } catch (error) {
    console.log(
      'Error occurred in subCategory getByCategoryIdAndBomConfigurationId dao',
      error
    );
    throw error;
  }
};

const updateBudget = async (
  actual_budget: number,
  sub_category_id: number,
  updated_by: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj ? connectionObj : prisma;
    const currentDate = new Date();
    const subCategoryDetails = await transaction.sub_category.update({
      where: {
        sub_category_id: sub_category_id,
      },
      data: {
        actual_budget: actual_budget,
        updated_date: currentDate,
        updated_by,
      },
    });

    return subCategoryDetails;
  } catch (error) {
    console.log('Error occurred in sub category dao updateBudget', error);
    throw error;
  }
};

const getSumOfBudgetByCategoryId = async (
  category_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.aggregate({
      where: {
        category_id: Number(category_id),
        is_delete: false,
      },
      _sum: {
        actual_budget: true,
      },
    });

    return subCategory._sum.actual_budget || 0;
  } catch (error) {
    console.error(
      'Error occurred in sub category dao getSumOfBudgetByCategoryId:',
      error
    );
    throw error;
  }
};

const getByParentSubCategoryId = async (
  parent_sub_category_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.findMany({
      where: {
        parent_sub_category_id: Number(parent_sub_category_id),
        is_delete: false,
      },
      include: {
        children: true,
        category: true,
        project_data: {
          select: {
            project_name: true,
            description: true,
          },
        },
        bom_configuration_data: {
          include: {
            bom_type_data: {
              select: {
                master_data_name: true,
              },
            },
          },
        },
        bom_detail: {
          where: {
            is_delete: false,
          },
        },
        uom_data: {
          select: { name: true },
        },
      },
      orderBy: [{ created_date: 'asc' }],
    });
    const response = subCategory.map((bomDetail) => ({
      ...bomDetail,
      is_bom_detail: bomDetail.bom_detail.length > 0 ? true : false,
    }));

    return response;
  } catch (error) {
    console.log(
      'Error occurred in subCategory getByParentSubCategoryId dao',
      error
    );
    throw error;
  }
};

const getParentSubCategoriesBySubCategoryId = async (
  sub_category_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : db;
    const query = `with recursive get_respective_parent_sc as (
                  select
                    sub_category_id,
                    name,
                    parent_sub_category_id,
                    actual_budget,
                    is_delete 
                  from
                    sub_category
                  where
                    sub_category_id = $1
                  union all
                  select
                    sc.sub_category_id,
                    sc.name,
                    sc.parent_sub_category_id,
                    sc.actual_budget,
                    sc.is_delete 
                  from
                    sub_category sc
                  inner join
                      get_respective_parent_sc sh on
                    sc.sub_category_id = sh.parent_sub_category_id
                  )
                  select
                    sub_category_id,
                    name,
                    actual_budget,
                    parent_sub_category_id,
                    is_delete 
                  from
                    get_respective_parent_sc
                  where
                    is_delete = false`;
    const data = transaction.manyOrNone(query, [sub_category_id]);
    return data;
  } catch (error) {
    console.error(
      'Error occurred in sub category dao getParentSubCategoriesBySubCategoryId:',
      error
    );
    throw error;
  }
};

const getParentSubCategoryBudgetByCategoryId = async (
  category_id: number,
  connectionObj = null
) => {
  try {
    const transaction = connectionObj !== null ? connectionObj : prisma;
    const subCategory = await transaction.sub_category.findFirst({
      where: {
        category_id: Number(category_id),
        is_delete: false,
        parent_sub_category_id: null,
      },
    });

    return subCategory.actual_budget || 0;
  } catch (error) {
    console.error(
      'Error occurred in sub category dao getSumOfBudgetByCategoryId:',
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
  searchSubCategory,
  getByCategoryId,
  updateBudget,
  getSumOfBudgetByCategoryId,
  getByCategoryIdAndBomConfigurationId,
  getByParentSubCategoryId,
  getParentSubCategoriesBySubCategoryId,
  getParentSubCategoryBudgetByCategoryId,
};
