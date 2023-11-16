import addProduct from '../service/add-product';
import { useQuery, useMutation, useQueryClient } from 'react-query';

const useCreateItem = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return addProduct.addProduct(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllPaginatedItemData']);
      },
    }
  );
};

const useGetAllItem = () => {
  return useQuery(['useGetAllItem'], () => addProduct.getAllItems(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useUpdateItem = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return addProduct.updateItem(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllPaginatedItemData']);
      },
    }
  );
};

const useGetAllPaginatedItemData = (data: any) => {
  return useQuery(
    ['useGetAllPaginatedItemData'],
    () => addProduct.filterItem(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useGetByItem = () => {
  return useMutation((data: any) => {
    return addProduct.filterItem(data);
  });
};

export {
  useCreateItem,
  useUpdateItem,
  useGetByItem,
  useGetAllItem,
  useGetAllPaginatedItemData,
};
