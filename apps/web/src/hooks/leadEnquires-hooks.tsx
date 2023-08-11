import { useQuery, useMutation, useQueryClient } from 'react-query';
import leadEnquiryService from '../service/leadEnquires-services';

const useGetAllleadEnquiry = () => {
  return useQuery(
    ['useGetAllleadEnquiry'],
    () => leadEnquiryService.getAllleadEnquiry(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const getByleadEnquiryID = (id: number) => {
  return useQuery(
    ['getOneleadEnquiryID', id],
    () => leadEnquiryService.getOneleadEnquiryByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const createleadEnquiry = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return leadEnquiryService.createleadEnquiry(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllleadEnquiry']);
      },
    }
  );
};

const updateleadEnquiry = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return leadEnquiryService.updateleadEnquiry(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllleadEnquiry']);
      },
    }
  );
};

const useDeleteleadEnquiry = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return leadEnquiryService.deleteleadEnquiry(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllleadEnquiry']);
      },
    }
  );
};

const getBySearchLeadEnquiry = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return leadEnquiryService.filterLeadEnquiry(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};
export {
  useGetAllleadEnquiry,
  getByleadEnquiryID,
  createleadEnquiry,
  updateleadEnquiry,
  useDeleteleadEnquiry,
  getBySearchLeadEnquiry,
};
