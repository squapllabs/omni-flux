import { useQuery,useQueryClient,useMutation } from 'react-query';
import ItemService from '../service/item-service';

const useGetAllItems = () => {
  return useQuery(['useGetAllItems'], () => ItemService.getAllItems(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useGetAllItemsDrops = () => {
  return useQuery(['useGetAllItemsDrop'], () => ItemService.getAllItems(), {
    select: (data) =>
      data?.data?.map((product: any) => ({
        value: product.item_id,
        label: product.item_name,
        temp: product,
      })),
  });
};

const instantCreateItem = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return ItemService.createItem(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllItemsDrop']);
      },
    }
  );
};

export { useGetAllItems, useGetAllItemsDrops,instantCreateItem };
