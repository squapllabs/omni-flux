-- Indent Request Details

alter table indent_request_details 
rename column quantity to indent_requested_quantity;

alter table indent_request_details 
add column purchase_requested_quantity int4;

alter table indent_request_details 
add column purchase_remaining_quantity int4;

-- Purchase Order Item

alter table purchase_order_item 
add column inward_quantity int4;

alter table purchase_order_item 
add column inward_remaining_quantity int4;

-- Expense Details

alter table expense_details 
add column bill_type varchar(20);

alter table expense_details 
add column is_recalled boolean default false;
