import { useQuery, useMutation, useQueryClient } from 'react-query';
import siteExpenseService from '../service/expense-service';

const useGetAllsiteExpense = () => {
  return useQuery(
    ['useGetAllsiteExpense'],
    () => siteExpenseService.getAllsiteExpense(),
    {
      select: (data) => data.data,
      staleTime: Infinity,
    }
  );
};

const useGetBysiteExpenseID = (id: number) => {
  return useQuery(
    ['getBysiteExpenseID', id],
    () => siteExpenseService.getOnesiteExpenseByID(id),
    {
      select: (data) => data.data,
    }
  );
};

const useGetBysiteExpenseCode = (code: any) => {
  return useQuery(
    ['getBysiteExpenseCode', code],
    () => siteExpenseService.getOnesiteExpenseByCode(code),
    {
      select: (data) => data.data,
    }
  );
};

const useCreatesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.createsiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useUpdatesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.updatesiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useCreateGlobalExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.createGlobalExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getBySearchsiteExpense']);
      },
    }
  );
};

const useUpdateGlobalExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.updateGlobalExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getBySearchsiteExpense']);
      },
    }
  );
};

const useUpdatesiteExpenseStatus = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.updatesiteExpenseStatus(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getBysiteExpenseID']);
      },
    }
  );
};

const useUpdatesiteExpenseDetail = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.updatesiteExpenseDetail(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['getBysiteExpenseID']);
      },
    }
  );
};

const useDeletesiteExpense = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.deletesiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};

const useGetBySearchsiteExpense = () => {
  // const queryClient = useQueryClient();
  return useMutation((data: any) => {
    return siteExpenseService.filtersiteExpense(data);
  });
};

const useGetAllPaginatedExpense = (data: any) => {
  return useQuery(
    ['useGetAllPaginatedExpense'],
    () => siteExpenseService.filtersiteExpense(data),
    {
      select: (data) => data,
      staleTime: Infinity,
    }
  );
};

const useBulkuploadSiteExpanse = () => {
  const queryClient = useQueryClient();
  return useMutation(
    (data: any) => {
      return siteExpenseService.createsiteExpense(data);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['useGetAllsiteExpense']);
      },
    }
  );
};
export {
  useGetAllsiteExpense,
  useGetBysiteExpenseID,
  useCreatesiteExpense,
  useUpdatesiteExpense,
  useDeletesiteExpense,
  useGetBySearchsiteExpense,
  useBulkuploadSiteExpanse,
  useUpdatesiteExpenseDetail,
  useGetAllPaginatedExpense,
  useUpdatesiteExpenseStatus,
  useGetBysiteExpenseCode,
  useCreateGlobalExpense,
  useUpdateGlobalExpense,
};
