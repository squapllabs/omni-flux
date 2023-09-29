alter table sub_category 
add column progress_status varchar(50);

alter table purchase_order 
add column payment_mode varchar(50);

alter table purchase_order 
add column payment_date date;