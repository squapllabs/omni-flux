import { useQuery, useMutation, useQueryClient } from 'react-query';
import purchaseRequestService from '../service/purchase-request.service';

const useGetOnePurchaseRequest = (id: any) => {
  return useQuery(
    ['useGetPurchaseRequest', id],
    () => purchaseRequestService.getOnePurchaseRequest(id),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetOnePurchaseOrder = (id: any) => {
  return useQuery(
    ['useGetPurchaseOrder', id],
    () => purchaseRequestService.getOnePurchaseOrderDataByID(id),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetOnePurchaseOrderTableData = (id: any) => {
  return useQuery(
    ['useGetPurchaseOrder', id],
    () => purchaseRequestService.getOnePurchaseOrderDataByID(id),
    {
      select: (data) =>
        data?.data?.purchase_order_item?.map((value: any) => ({
          item_id: value.item_id,
          item_name: value.item_data.item_name,
          requested_quantity: value.order_quantity,
          inward_quantity: value.inward_quantity,
          unit_price: value.unit_price,
        })),
      staleTime: Infinity,
    }
  );
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

const useGetOneOrderPurchaseRequest = (id: any) => {
  return useQuery(
    ['useGetOrderPurchaseRequestSample', id],
    () => purchaseRequestService.getOneOrderPurchaseRequest(id),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
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
        queryClient.invalidateQueries(['useGetAllPOPaginatedData']);
      },
    }
  );
};

const updatePurchseOrderStatus = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return purchaseRequestService.updatePoStatus(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllPOPaginatedData']);
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

const useGetAllMyOrderData = (data: any) => {
  return useQuery(
    ['useGetAllMyOrderData'],
    () => purchaseRequestService.getMyOrdersData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const getBySearchPoData = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return purchaseRequestService.getPoData(data);
    },
    {
      onSuccess: (response) => {
        response;
      },
    }
  );
};
// const getBySearchPR = () => {
//   const queryClient = useQueryClient();
//   return useMutation(
//     (data: any) => {
//       return purchaseRequestService.purchaseDetailData(data);
//     },
//     {
//       onSuccess: (response) => {
//         response;
//       },
//     }
//   );
// };
const getBySearchPR = (data: any) => {
  return useQuery(
    ['getAllPRbasedOnIndent'],
    () => purchaseRequestService.purchaseDetailData(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const purchaseOrderGetAll = (data: any) => {
  return useQuery(
    ['useGetAllpurchaseOrder'],
    () => purchaseRequestService.purchseOrderGetAll(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

export {
  useGetOnePurchaseRequest,
  purchaseOrderRequest,
  useGetOneOrderPurchaseRequest,
  useGetMasterBillStatusParentType,
  updatePurchseOrderBillStatus,
  useGetAllPurchaseOrderData,
  getBySearchPoData,
  useGetOnePurchaseOrder,
  getBySearchPR,
  useGetOnePurchaseOrderTableData,
  purchaseOrderGetAll,
  useGetAllMyOrderData,
  updatePurchseOrderStatus,
};
