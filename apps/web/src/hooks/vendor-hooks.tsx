import { useQuery, useMutation, useQueryClient } from 'react-query';
import vendorService from '../service/vendor-service'

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

  const getByFilterVendor = () => {
    const queryClient = useQueryClient();
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

  const createVendor = () => {
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

  const updateVendor = () => {
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

  const getByVendorId = (id: number) => {
    return useQuery(
      ['getByuserID', id],
      () => vendorService.getOneVendorById(id),
      {
        select: (data) => data.data,
      }
    );
  };

  export {
    useGetAllPaginatedVendor,
    getByFilterVendor,
    useDeleteVendor,
    createVendor,
    updateVendor,
    getByVendorId
  }