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

const UseCreateInstantCategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return CategoryService.createCategory(data);
    },
    {
      onSuccess: (data, _v) => {
        // console.log('Test data', _v);
        queryClient.invalidateQueries(['useGetAllCategoryByProject']);
        queryClient.invalidateQueries([
          'getBOMDetails',
          { projectId: _v.project_id, boQId: _v.bom_configuration_id },
        ]);
      },
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

const useGetByCategoryID = (id: number) => {
  return useQuery(
    ['getOnecategoryID', id],
    () => CategoryService.getOneCategoryByID(id),
    {
      select: (data) => data.data,
    }
  );
};
const useGetByBOMDetails = (value: any) => {
  return useQuery(
    ['getBOMDetails', value],
    () => CategoryService.getBOMDetail(value),
    {
      select: (data) => data.data,
    }
  );
};

const useCreateCategory = () => {
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
const useCreateMultipleCategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return CategoryService.createMultipleCategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllCategory']);
      },
    }
  );
};

const useUpdateCategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return CategoryService.updateCategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllCategoryByProject']);
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
        queryClient.invalidateQueries(['useGetAllCategoryByProject']);
      },
    }
  );
};
const useGetBySearchCategroy = () => {
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
const useGetMasterAbstractStatusParentType = () => {
  return useQuery(
    ['useGetAllAbstractStatusType'],
    () => CategoryService.getAllAbstractStatusParentType(),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project.master_data_name,
          label: project.master_data_name,
        })),
    }
  );
};
export {
  useGetAllCategory,
  useGetByCategoryID,
  useCreateCategory,
  useUpdateCategory,
  useDeleteCategory,
  useGetAllCategoryForDrop,
  useGetBySearchCategroy,
  useGetAllCategoryByProjectId,
  UseCreateInstantCategory,
  useGetMasterAbstractStatusParentType,
  useGetByBOMDetails,
  useCreateMultipleCategory
};
