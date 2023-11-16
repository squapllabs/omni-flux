import { useQuery, useMutation, useQueryClient } from 'react-query';
import SubSubcategoryService from '../service/subSubCategory-service';

const useGetAllSubSubcategory = () => {
  return useQuery(
    ['useGetAllSubSubcategory'],
    () => SubSubcategoryService.getAllSubSubcategory(),
    {
      select: (data) => data.data,
    }
  );
};

const useGetBySubSubcategoryID = (id: number) => {
  return useQuery(
    ['getOneSubcategoryID', id],
    () => SubSubcategoryService.getOneSubSubcategoryByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const useCreateSubSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubSubcategoryService.createSubSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllSubSubcategory']);
      },
    }
  );
};

const useUpdateSubSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubSubcategoryService.updateSubSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllSubSubcategory']);
      },
    }
  );
};

const useDeleteSubSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubSubcategoryService.deleteSubSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllSubSubcategory']);
      },
    }
  );
};

const useGetBySearchSubSubCategroy = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubSubcategoryService.filterSubSubCategory(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};
export {
  useGetAllSubSubcategory,
  useGetBySubSubcategoryID,
  useCreateSubSubcategory,
  useUpdateSubSubcategory,
  useDeleteSubSubcategory,
  useGetBySearchSubSubCategroy,
};
