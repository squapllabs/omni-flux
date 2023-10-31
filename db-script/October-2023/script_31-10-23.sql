alter table purchase_order 
add column purchase_order_type varchar(50);

alter table purchase_order 
add column indent_request_id int4;

alter table purchase_order 
add constraint fk_purchase_order_indent_request_id foreign key (indent_request_id) references indent_request(indent_request_id);

