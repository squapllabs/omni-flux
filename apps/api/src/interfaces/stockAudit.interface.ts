interface stockAuditBody {
  stock_audit_id: number;
  project_id: number;
  site_id: number;
  stock_audit_date: Date;
  item_details: JSON;
  created_by: number;
  updated_by: number;
}

export { stockAuditBody };
