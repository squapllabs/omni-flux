import { useQuery, useMutation, useQueryClient } from 'react-query';
import SubcategoryService from '../service/subCategory-service';

const useGetAllSubcategory = () => {
  return useQuery(
    ['useGetAllSubcategory'],
    () => SubcategoryService.getAllSubcategory(),
    {
      select: (data) => data.data,
    }
  );
};

const getBySubcategoryID = (id: number) => {
  return useQuery(
    ['getOneSubcategoryID', id],
    () => SubcategoryService.getOneSubcategoryByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createSubcategory = () => {
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

const updateSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.updateSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllSubcategory']);
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
        queryClient.invalidateQueries(['useGetAllSubcategory']);
      },
    }
  );
};

export {
  useGetAllSubcategory,
  getBySubcategoryID,
  createSubcategory,
  updateSubcategory,
  useDeleteSubcategory,
};
