import { useQuery, useMutation, useQueryClient } from 'react-query';
import GrnService from '../service/grn-service';

const useCreateGrn = () => {
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

const useGetOneGrnById = (id: any) => {
  return useQuery(
    ['useGetOneGrnById', id],
    () => GrnService.getGrnById(id),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

export { useCreateGrn,useGetAllGrnData,useGetOneGrnById };
