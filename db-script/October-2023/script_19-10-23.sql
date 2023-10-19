alter table indent_request_details 
rename column quantity to indent_requested_quantity;

alter table indent_request_details 
add column purchase_requested_quantity int4;

alter table indent_request_details 
add column purchase_remaining_quantity int4;

alter table purchase_order_item 
add column inward_quantity int4;

alter table purchase_order_item 
add column inward_remaining_quantity int4;