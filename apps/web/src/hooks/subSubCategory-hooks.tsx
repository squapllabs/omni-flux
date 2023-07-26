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

const getBySubSubcategoryID = (id: number) => {
  return useQuery(
    ['getOneSubcategoryID', id],
    () => SubSubcategoryService.getOneSubSubcategoryByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createSubSubcategory = () => {
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

const updateSubSubcategory = () => {
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

export {
    useGetAllSubSubcategory,
    getBySubSubcategoryID,
    createSubSubcategory,
    updateSubSubcategory,
    useDeleteSubSubcategory,
};
