alter table purchase_order_item 
drop column purchase_order_item_documents;

alter table purchase_order 
add column purchase_order_documents jsonb;

alter table vendor_quotes 
add column quotation_id varchar(100);

select concat('VQUO',DATE_PART('year', CURRENT_DATE),'00',nextval('vendor_quotation_sequence')::text) as vendor_quotation_sequence
