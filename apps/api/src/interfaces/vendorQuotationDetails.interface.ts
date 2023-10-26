interface vendorQuotationDetailsBody {
  vendor_quotation_details_id: number;
  vendor_quotes_id: number;
  item_id: number;
  indent_request_details_id: number;
  indent_requested_quantity: number;
  purchase_requested_quantity: number;
  unit_cost: number;
  total_cost: number;
  is_delete: boolean;
  created_by: number;
  updated_by: number;
}

export { vendorQuotationDetailsBody };
