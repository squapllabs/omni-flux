
alter table item 
drop constraint fk_item_type_id;

alter table item 
add constraint fk_item_item_type_id foreign key (item_type_id) references master_data(master_data_id);