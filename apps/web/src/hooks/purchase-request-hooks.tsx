import { id } from 'date-fns/esm/locale';
import { useQuery, useMutation, useQueryClient } from 'react-query';
import purchaseRequestService from '../service/purchase-request.service';


const useGetOnePurchaseRequest = (id : any)  => {
    return useQuery(['useGetAll',id], () => purchaseRequestService.getOnePurchaseRequest(id), {
      select: (data) => data.data,
      staleTime: Infinity,
    });
  };

  export { useGetOnePurchaseRequest };
