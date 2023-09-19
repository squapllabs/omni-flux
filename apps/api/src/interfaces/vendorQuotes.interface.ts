interface vendorQuotesBody {
    vendor_quotes_id: number;
    vendor_id: number;
    purchase_request_id: number;
    quotation_date: Date;
    quotation_status: string;
    total_quotation_amount: number;
    remarks: string;
    quotation_details: JSON;
    created_by: number;
    updated_by: number;
}

export { vendorQuotesBody }