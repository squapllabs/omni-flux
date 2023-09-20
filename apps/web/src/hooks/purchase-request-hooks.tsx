import { useQuery, useMutation, useQueryClient } from 'react-query';
import purchaseRequestService from '../service/purchaseRequest-service';

const updatePurchaseRequest = () => {
    const queryClient = useQueryClient();
    return useMutation(
      (data: any) => {
        return purchaseRequestService.updatePurchaseRequest(data);
      },
      {
        onSuccess: (data, _v) => {
          queryClient.invalidateQueries(['getSubcategoryList'], _v.category_id);
        },
      }
    );
  };

  export {updatePurchaseRequest};