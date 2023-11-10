interface vendorBody {
  vendor_id: number;
  vendor_name: string;
  contact_person: string;
  contact_email: string;
  contact_phone_no: string;
  address: JSON;
  tax_id: string;
  payment_terms: string;
  preferred_payment_method_id: number;
  bank_account_details: JSON;
  currency: string;
  vendor_category_id: number;
  lead_time: string;
  minimum_order_quantity: number;
  notes: string;
  created_by: number;
  updated_by: number;
  ratings: string;
}

export { vendorBody };
