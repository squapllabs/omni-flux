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

  export { createPurchaseRequest };