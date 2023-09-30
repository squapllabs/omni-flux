import { useQuery, useMutation, useQueryClient } from 'react-query';
import PurchaseRequestService from '../service/purchaseRequest-service';


const createPurchaseRequest = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return PurchaseRequestService.addPurchaseRequest(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['']);
      },
    }
  );
};

const useGetAllPaginatedPurchaseRequests = (data: any) => {
  return useQuery(
    ['useGetAllPaginatedPurchaseRequests'],
    () => PurchaseRequestService.purchaseDetailData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
}

const getBySearchPurchaseRequestes = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return PurchaseRequestService.purchaseDetailData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};

  export { createPurchaseRequest,useGetAllPaginatedPurchaseRequests,getBySearchPurchaseRequestes };