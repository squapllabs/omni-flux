import { useQuery, useMutation, useQueryClient } from 'react-query';
import hsnCodeService from '../service/hsnCode-service';

const useGetAllHsnCode = () => {
  return useQuery(['useGetAllHsnCode'], () => hsnCodeService.getAllHsnCode(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useGetAllHsnForDrop = () => {
  return useQuery(['useGetAllHsnDrop'], () => hsnCodeService.getAllHsnCode(), {
    select: (data) =>
      data?.data?.map((hsn: any) => ({
        value: hsn.hsn_code_id,
        label: hsn.code,
      })),
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
        queryClient.invalidateQueries(['useGetAllhsnCodeData']);
      },
    }
  );
};

const useCreateHsnCode = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return hsnCodeService.createHsnCode(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllhsnCodeData']);
      },
    }
  );
};

const useUploadHsnCode = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return hsnCodeService.uploadHsnCode(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllhsnCodeData']);
      },
    }
  );
};

const useUpdateHsnCode = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return hsnCodeService.updateHsnCode(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllhsnCodeData']);
      },
    }
  );
};
const useGetByCode = () => {
  return useMutation((data: any) => {
    return hsnCodeService.filterHsn(data);
  });
};

const useGetAllPaginatedHsnCodeData = (data: any) => {
  return useQuery(
    ['useGetAllhsnCodeData'],
    () => hsnCodeService.filterHsn(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

export {
  useGetAllHsnCode,
  useDeleteHsnCode,
  useCreateHsnCode,
  useUpdateHsnCode,
  useUploadHsnCode,
  useGetByCode,
  useGetAllPaginatedHsnCodeData,
  useGetAllHsnForDrop,
};
