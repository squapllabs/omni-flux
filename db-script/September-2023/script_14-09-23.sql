alter table inventory 
drop column item_id;

alter table vendor_quotes 
rename column quatation_details to quotation_details;