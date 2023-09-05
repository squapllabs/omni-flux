import { useQuery, useMutation, useQueryClient } from 'react-query';
import CategoryService from '../service/category-service';

const useGetAllCategory = () => {
  return useQuery(
    ['useGetAllCategory'],
    () => CategoryService.getAllCategory(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};
const useGetAllCategoryByProjectId = (id: number) => {
  return useQuery(
    ['useGetAllCategoryByProject'],
    () => CategoryService.getAllCategoryByProjectId(id),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};
const useGetAllCategoryForDrop = () => {
  return useQuery(
    ['useGetAllCategoryDrop'],
    () => CategoryService.getAllCategory(),
    {
      select: (data) =>
        data?.data?.map((category: any) => ({
          value: category.category_id,
          label: category.name,
        })),
    }
  );
};

const getByCategoryID = (id: number) => {
  return useQuery(
    ['getOnecategoryID', id],
    () => CategoryService.getOneCategoryByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createCategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return CategoryService.createCategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllCategory']);
      },
    }
  );
};

const updateCategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return CategoryService.updateCategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllCategory']);
      },
    }
  );
};

const useDeleteCategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return CategoryService.deleteCategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllCategory']);
      },
    }
  );
};
const getBySearchCategroy = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return CategoryService.filterCategory(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};
export {
  useGetAllCategory,
  getByCategoryID,
  createCategory,
  updateCategory,
  useDeleteCategory,
  useGetAllCategoryForDrop,
  getBySearchCategroy,
  useGetAllCategoryByProjectId
};
