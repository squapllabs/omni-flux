import { useQuery, useMutation, useQueryClient } from 'react-query';
import StockAuditService from '../service/stockaudit-service';

const useGetAllPaginatedStockAudit = (data: any) => {
  return useQuery(
    ['useGetAllPaginatedStockAudit'],
    () => StockAuditService.filterStockAudit(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useGetByFilterStockAudit = () => {
  return useMutation((data: any) => {
    return StockAuditService.filterStockAudit(data);
  });
};

const useDeleteStockAudit = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return StockAuditService.deleteStockAudit(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllStockAuditData']);
      },
    }
  );
};

const useCreateStockAudit = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return StockAuditService.createStockAudit(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllPaginatedStockAudit']);
      },
    }
  );
};

const useUpdateStockAudit = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return StockAuditService.updateStockAudits(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllStockAuditData']);
      },
    }
  );
};

const useGetByStockAuditId = (id: number) => {
  return useQuery(
    ['getByStockAuditId', id],
    () => StockAuditService.getOneStockAuditById(id),
    {
      select: (data) => data.data,
    }
  );
};

const useGetAllStockAudits = () => {
  return useQuery(
    ['useGetAllStockAudits'],
    () => StockAuditService.getAllStockAudits(),
    {
      select: (data) =>
        data?.data?.map((StockAudit: any) => ({
          value: StockAudit.StockAudit_id,
          label: StockAudit.StockAudit_name,
        })),
      refetchOnMount: true,
      refetchOnWindowFocus: true,
      staleTime: 60000,
    }
  );
};

const useGetItemByProjectAndSite = (values: any) => {
  return useQuery(
    ['getItems', values],
    () => StockAuditService.getItems(values),
    {
      select: (data) => data.data,
    }
  );
};

export {
  useGetAllPaginatedStockAudit,
  useGetByFilterStockAudit,
  useDeleteStockAudit,
  useCreateStockAudit,
  useUpdateStockAudit,
  useGetByStockAuditId,
  useGetAllStockAudits,
  useGetItemByProjectAndSite,
};
