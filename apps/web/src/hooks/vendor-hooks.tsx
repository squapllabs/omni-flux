import { useQuery, useMutation, useQueryClient } from 'react-query';
import vendorService from '../service/vendor-service';

const useGetAllPaginatedVendor = (data: any) => {
  return useQuery(
    ['useGetAllVendorData'],
    () => vendorService.filterVendor(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useGetByFilterVendor = () => {
  return useMutation(
    (data: any) => {
      return vendorService.filterVendor(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

const useDeleteVendor = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return vendorService.deleteVendor(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllVendorData']);
      },
    }
  );
};

const useCreateVendor = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return vendorService.createVendor(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllVendorData']);
      },
    }
  );
};

const useUpdateVendor = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return vendorService.updateVendors(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllVendorData']);
      },
    }
  );
};

const useGetByVendorId = (id: number) => {
  return useQuery(
    ['getByuserID', id],
    () => vendorService.getOneVendorById(id),
    {
      select: (data) => data.data,
    }
  );
};

const useGetAllVendors = () => {
  return useQuery(['useGetAllVendors'], () => vendorService.getAllVendors(), {
    select: (data) =>
      data?.data?.map((vendor: any) => ({
        value: vendor.vendor_id,
        label: vendor.vendor_name,
      })),
    refetchOnMount: true,
    refetchOnWindowFocus: true,
    staleTime: 60000,
  });
};

export {
  useGetAllPaginatedVendor,
  useGetByFilterVendor,
  useDeleteVendor,
  useCreateVendor,
  useUpdateVendor,
  useGetByVendorId,
  useGetAllVendors,
};
