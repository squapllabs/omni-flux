import { useQuery, useMutation, useQueryClient } from 'react-query';
import LabourService from '../service/labour-service';

const useGetAllLabourForDrop = () => {
  return useQuery(
    ['useGetAllLabourDrop'],
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

const getBySearchLabour = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return LabourService.filterLabour(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
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

const createLabour = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return LabourService.addLabour(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};

const createInstantLabour = () => {
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

const updateLabour = () => {
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

export { useGetAllLabourForDrop, getBySearchLabour, useGetLabourUomForDrop, createLabour, updateLabour, useDeleteLabour, useGetAllLabour,createInstantLabour };
