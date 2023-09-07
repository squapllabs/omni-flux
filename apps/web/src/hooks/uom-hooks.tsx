import { useQuery, useMutation, useQueryClient } from 'react-query';
import uomService from '../service/uom-service';

const useGetAlluom = () => {
  return useQuery(['useGetAlluom'], () => uomService.getAlluom(), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};

const getByuserID = (id: number) => {
  return useQuery(['getOneUomByID', id], () => uomService.getOneUomByID(id), {
    select: (data) => data.data,
  });
};
const getUomByType = (type: any) => {
  return useQuery(
    ['getUomByTypeID', type],
    () => uomService.getUomByType(type),
    {
      select: (data) =>
        data?.data?.map((uom: any) => ({
          value: uom.uom_id,
          label: uom.name,
          data: uom,
        })),
    }
  );
};

const createuom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return uomService.createUom(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUomData']);
      },
    }
  );
};

const updateUom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return uomService.updateUom(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUomData']);
      },
    }
  );
};

const useDeleteUom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return uomService.deleteUom(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllUomData']);
      },
    }
  );
};
const getByUom = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return uomService.filterUom(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useGetAllUomDrop = () => {
  return useQuery(['useGetAlluom'], () => uomService.getAlluom(), {
    select: (data) =>
      data?.data?.map((uom: any) => ({
        value: uom.uom_id,
        label: uom.name,
      })),
  });
};

const useGetAllPaginatedUomData = (data: any) => {
  return useQuery(['useGetAllUomData'], () => uomService.filterUom(data), {
    select: (data) => data,
    staleTime: Infinity,
  });
};

export {
  useGetAlluom,
  getByuserID,
  createuom,
  updateUom,
  useDeleteUom,
  useGetAllUomDrop,
  getByUom,
  useGetAllPaginatedUomData,
  getUomByType,
};
