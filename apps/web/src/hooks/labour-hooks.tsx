import { useQuery, useMutation, useQueryClient } from 'react-query';
import LabourService from '../service/labour-service';

const useGetAllLabourForDrop = () => {
  return useQuery(
    ['useGetAllLabourForDrop'],
    () => LabourService.getAllLabours(),
    {
      select: (data) =>
        data?.data?.map((option: any) => ({
          value: option.labour_id,
          label: option.labour_type,
          data: option,
        })),
    }
  );
};

const useGetBySearchLabour = () => {
  return useMutation((data: any) => {
    return LabourService.filterLabour(data);
  });
};

const useGetAllLabour = (data: any) => {
  return useQuery(
    ['useGetAllLabourData'],
    () => LabourService.filterLabour(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useGetLabourUomForDrop = () => {
  return useQuery(
    ['useGetLabourUomDrop'],
    () => LabourService.getLaboursUom(),
    {
      select: (data) =>
        data?.data?.map((option: any) => ({
          value: option.uom_id,
          label: option.name,
          data: option,
        })),
    }
  );
};

const useCreateLabour = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return LabourService.addLabour(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllLabourData']);
      },
    }
  );
};

const useCreateInstantLabour = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return LabourService.addLabour(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllLabourForDrop']);
      },
    }
  );
};

const useUpdateLabour = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return LabourService.editLabour(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllLabourData']);
      },
    }
  );
};

const useDeleteLabour = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return LabourService.deleteLabour(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllLabourData']);
      },
    }
  );
};

export {
  useGetAllLabourForDrop,
  useGetBySearchLabour,
  useGetLabourUomForDrop,
  useCreateLabour,
  useUpdateLabour,
  useDeleteLabour,
  useGetAllLabour,
  useCreateInstantLabour,
};
