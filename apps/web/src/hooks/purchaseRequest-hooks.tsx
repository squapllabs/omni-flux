import { useMutation, useQueryClient } from 'react-query';
import PurchaseRequestService from '../service/purchaseRequest-service';

const useCreatePurchaseRequest = () => {
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

export { useCreatePurchaseRequest };
