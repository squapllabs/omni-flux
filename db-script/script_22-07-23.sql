-- is_delete implementation in category , sub_category and sub_sub_category

alter table category 
add column is_delete boolean default false;

alter table sub_category 
add column is_delete boolean default false;

alter table sub_sub_category 
add column is_delete boolean default false;