interface purchaseOrderInvoiceBody {
  purchase_order_invoice_id: number;
  purchase_order_id: number;
  grn_id: number;
  invoice_number: string;
  invoice_document: JSON;
  requested_by: number;
  invoice_date: Date;
  due_date: Date;
  status: string;
  additional_info: JSON;
  total_amount: number;
  paid_by: number;
  paid_date: Date;
  created_by: number;
  updated_by: number;
}
export { purchaseOrderInvoiceBody };
