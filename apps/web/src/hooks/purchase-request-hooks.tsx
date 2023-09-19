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
  return useQuery(['useGetOrderPurchaseRequestSample',id], () => purchaseRequestService.getOneOrderPurchaseRequest(id), {
    select: (data) => data.data,
    staleTime: Infinity,
  });
};
  
const useGetMasterBillStatusParentType = () => {
  return useQuery(
    ['useGetAllBillStatusType'],
    () => purchaseRequestService.getAllBillStatusParentType(),
    {
      select: (data) =>
        data?.data?.map((project: any) => ({
          value: project.master_data_name,
          label: project.master_data_name,
        })),
    }
  );
};

const updatePurchseOrderBillStatus = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return purchaseRequestService.updatePoBillStatus(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllMasterPaginatedData']);
      },
    }
  );
};

const useGetAllPurchaseOrderData = (data: any) => {
  return useQuery(
    ['useGetAllPOPaginatedData'],
    () => purchaseRequestService.getPoData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};
  export { useGetOnePurchaseRequest,purchaseOrderRequest,useGetOneOrderPurchaseRequest,useGetMasterBillStatusParentType,updatePurchseOrderBillStatus,useGetAllPurchaseOrderData };
