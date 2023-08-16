alter table users 
add column parent_user_id int4;

alter table users 
add constraint fk_users_parent_user_id foreign key (parent_user_id) references users(user_id);

alter table project 
add column approvar_id int4;

alter table project 
add constraint fk_project_approver_id foreign key (approvar_id) references users(user_id);

alter table project_site 
add column approvar_id int4;

alter table project_site
add constraint fk_project_site_approver_id foreign key (approvar_id) references users(user_id);

alter table site_expense_details 
add column status varchar(50);

alter table site_expense_details 
add column comments varchar(300);

alter table site_expense_details 
add column progressed_date date;

alter table site_expense_details 
add column progressed_by int4;

alter table site_expense_details
add constraint fk_site_expense_details_progressed_by foreign key (progressed_by) references users(user_id);

alter table site_expense_details 
alter column progressed_date type timestamptz;