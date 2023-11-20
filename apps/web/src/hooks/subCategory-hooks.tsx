import { useQuery, useMutation, useQueryClient } from 'react-query';
import SubcategoryService from '../service/subCategory-service';

const useGetAllSubcategory = () => {
  return useQuery(
    ['useGetAllSubcategory'],
    () => SubcategoryService.getAllSubcategory(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetAllSubcategoryDrop = () => {
  return useQuery(
    ['useGetAllSubcategory'],
    () => SubcategoryService.getAllSubcategory(),
    {
      select: (data) =>
        data?.data?.map((category: any) => ({
          value: category.sub_category_id,
          label: category.name,
        })),
    }
  );
};

const useGetBySubcategoryID = (id: number) => {
  return useQuery(
    ['getOneSubcategoryID', id],
    () => SubcategoryService.getOneSubcategoryByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const useCreateSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.createSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllSubcategory']);
      },
    }
  );
};
const useCreateMultipleSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.createMultipleSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllSubcategory']);
      },
    }
  );
};

const useGetBySearchCategroy = () => {
  return useMutation((data: any) => {
    return SubcategoryService.filterSubCategory(data);
  });
};

const useGetBycategoryIdInSub = (values: any) => {
  return useQuery(
    ['getSubcategoryList', values],
    () => SubcategoryService.getOneSubCatListbyCatID(values),
    {
      select: (data) => data.data,
    }
  );
};

const useCreateInstantSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.createSubcategory(data);
    },
    {
      onSuccess: (data, _v) => {
        console.log('createInstantSubcategory', _v);
        queryClient.invalidateQueries(['getSubcategoryList']);
        queryClient.invalidateQueries([
          'getBOMDetails',
          { projectId: _v.project_id, boQId: _v.bom_configuration_id },
        ]);
      },
    }
  );
};

const useUpdateSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.updateSubcategory(data);
    },
    {
      onSuccess: (data, _v) => {
        queryClient.invalidateQueries(['getSubcategoryList'], _v.category_id);
      },
    }
  );
};

const useDeleteSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.deleteSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getSubcategoryList']);
      },
    }
  );
};

export {
  useGetAllSubcategory,
  useGetBySubcategoryID,
  useCreateSubcategory,
  useCreateMultipleSubcategory,
  useUpdateSubcategory,
  useDeleteSubcategory,
  useGetBySearchCategroy,
  useGetAllSubcategoryDrop,
  useGetBycategoryIdInSub,
  useCreateInstantSubcategory,
};
