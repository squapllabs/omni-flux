interface vendorQuotesBody {
  vendor_quotes_id: number;
  vendor_id: number;
  purchase_request_id: number;
  quotation_date: Date;
  quotation_status: string;
  total_quotation_amount: number;
  remarks: string;
  vendor_quotation_details: Array<vendorQuotationDetails>;
  created_by: number;
  updated_by: number;
  quotation_id: string;
  vendor_quotes_documents: Array<vendorQuotesDocuments>;
}

interface vendorQuotesDocuments {
  path: string;
  is_delete: boolean;
}

interface vendorQuotationDetails {
  vendor_quotation_details_id: number;
  vendor_quotes_id: number;
  item_id: number;
  indent_request_details_id: number;
  indent_requested_quantity: number;
  purchase_requested_quantity: number;
  unit_cost: number;
  total_cost: number;
  updated_by: number;
}
export { vendorQuotesBody };
