import { id } from 'date-fns/esm/locale';
import { useQuery, useMutation, useQueryClient } from 'react-query';
import purchaseRequestService from '../service/purchase-request.service';


const useGetOnePurchaseRequest = (id : any)  => {
    return useQuery(['useGetPurchaseRequest',id], () => purchaseRequestService.getOnePurchaseRequest(id), {
      select: (data) => data.data,
      staleTime: Infinity,
    });
  };

  const purchaseOrderRequest = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return purchaseRequestService.createPurchaseOrderItem(data);
      },
      {
        onSuccess: () => {
          queryClient.invalidateQueries(['useGetOrderPurchaseRequest']);
        },
      }
    );
  };

  
const useGetOneOrderPurchaseRequest = (id : any)  => {
  return useQuery(['useGetOrderPurchaseRequest',id], () => purchaseRequestService.getOneOrderPurchaseRequest(id), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};
  

  export { useGetOnePurchaseRequest,purchaseOrderRequest,useGetOneOrderPurchaseRequest };
