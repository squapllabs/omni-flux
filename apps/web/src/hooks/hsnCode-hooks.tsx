import { useQuery, useMutation, useQueryClient } from 'react-query';
import hsnCodeService from '../service/hsnCode-service';

const useGetAllHsnCode = () => {
  return useQuery(['useGetAllHsnCode'], () => hsnCodeService.getAllHsnCode(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useDeleteHsnCode = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return hsnCodeService.deleteHsnCode(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllHsnCode']);
      },
    }
  );
};

const createHsnCode = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return hsnCodeService.createHsnCode(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllHsnCode']);
      },
    }
  );
};

const updateHsnCode = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return hsnCodeService.updateHsnCode(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllHsnCode']);
      },
    }
  );
};
export { useGetAllHsnCode, useDeleteHsnCode, createHsnCode, updateHsnCode };
