import { useQuery, useMutation, useQueryClient } from 'react-query';
import ItemService from '../service/item-service';

const useGetAllItems = () => {
  return useQuery(['useGetAllItems'], () => ItemService.getAllItems(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

export { useGetAllItems };
