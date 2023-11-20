import { useQuery } from 'react-query';
import InvoiceService from '../service/invoice-service';

const useGetByPurchaseOrderId = (id: number) => {
  return useQuery(
    ['getInvoiceData', id],
    () => InvoiceService.getOnePurchaseOrderById(id),
    {
      select: (data) => data.data,
    }
  );
};

export { useGetByPurchaseOrderId };
