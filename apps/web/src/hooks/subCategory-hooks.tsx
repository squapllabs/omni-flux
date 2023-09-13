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

const getBySearchCategroy = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.filterSubCategory(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const getBycategoryIdInSub = (values : any) => {
  return useQuery(
    ['getSubcategoryList', values],
    () => SubcategoryService.getOneSubCatListbyCatID(values),
    {
      select: (data) => data.data,
    }
  );
};

const createInstantSubcategory = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return SubcategoryService.createSubcategory(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getSubcategoryList']);
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
  getBySubcategoryID,
  createSubcategory,
  updateSubcategory,
  useDeleteSubcategory,
  getBySearchCategroy,
  useGetAllSubcategoryDrop,
  getBycategoryIdInSub,
  createInstantSubcategory
};
