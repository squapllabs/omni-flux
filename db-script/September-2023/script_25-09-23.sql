alter table expense 
add column comments varchar(300);

alter table expense 
add column progressed_date date;

alter table expense 
add column progressed_by int4;

alter table expense 
add constraint fk_expense_progressed_by foreign key (progressed_by) references users(user_id);

alter table expense 
drop column progressed_date ;   

alter table expense 
add column progressed_date timestamptz;

alter table expense_details 
add constraint fk_expense_details_progressed_by foreign key (progressed_by) references users(user_id);

alter table master_data 
add column project_id int4;

alter table master_data
add constraint fk_master_data_project_id foreign key (project_id) references project(project_id);