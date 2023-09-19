alter table purchase_request 
add column purchase_request_details jsonb;

alter table purchase_request
add column purchase_request_documents jsonb;