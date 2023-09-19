alter table purchase_order_item 
drop column purchase_order_item_documents;

alter table purchase_order 
add column purchase_order_documents jsonb;