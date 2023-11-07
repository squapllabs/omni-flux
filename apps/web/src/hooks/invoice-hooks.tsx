import { useQuery, useMutation, useQueryClient } from 'react-query';
import InvoiceService from '../service/invoice-service';

const getByPurchaseOrderId = (id: number) => {
  return useQuery(
    ['getInvoiceData', id],
    () => InvoiceService.getOnePurchaseOrderById(id),
    {
      select: (data) => data.data,
    }
  );
};

export { getByPurchaseOrderId };
