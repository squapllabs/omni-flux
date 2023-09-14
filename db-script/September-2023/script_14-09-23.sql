alter table inventory 
drop column item_id;

alter table vendor_quotes 
rename column quatation_details to quotation_details;

alter table indent_request 
rename column approvar_user_id to approver_user_id;

alter table indent_request 
rename column approvar_status to approver_status;

alter table indent_request 
rename column approvar_comments to approver_comments;