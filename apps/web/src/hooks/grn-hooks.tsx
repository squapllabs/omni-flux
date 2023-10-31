import { useQuery, useMutation, useQueryClient } from 'react-query';
import GrnService from '../service/grn-service';

const createGrn = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return GrnService.createGrnData(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries([]);
      },
    }
  );
};

const useGetAllGrnData = (data: any) => {
  return useQuery(
    ['useGetAllGrnData'],
    () => GrnService.filterGrn(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

export { createGrn,useGetAllGrnData };
