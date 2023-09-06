import { useQuery, useMutation, useQueryClient } from 'react-query';
import hsnCodeService from '../service/hsnCode-service';

const useGetAllHsnCode = () => {
  return useQuery(['useGetAllHsnCode'], () => hsnCodeService.getAllHsnCode(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const useGetAllHsnForDrop = () => {
  return useQuery(
      ['useGetAllHsnDrop'],
      () => hsnCodeService.getAllHsnCode(),
      {
          select: (data) =>
              data?.data?.map((hsn: any) => ({
                  value:hsn.hsn_code_id,
                  label:hsn.code,
              })),
      }
  );
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

const createHsnCode = () => {
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

const uploadHsnCode = () => {
  const queryClient = useQueryClient();
  return useMutation (
    (data: any) => {
      return hsnCodeService.uploadHsnCode(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllhsnCodeData']);
      },
    }
  );
}

const updateHsnCode = () => {
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
const getByCode = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return hsnCodeService.filterHsn(data);
    },
    {
      onSuccess: (response) => {
        
        response;
      },
    }
  );
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

export { useGetAllHsnCode, useDeleteHsnCode, createHsnCode, updateHsnCode ,uploadHsnCode, getByCode, useGetAllPaginatedHsnCodeData,useGetAllHsnForDrop};
