create table expense_recall(
expense_recall_id serial4 not null,
project_id int4,
site_id int4,
expense_id int4 not null,
expense_details_id int4 not null, 
recall_creator_id int4,
recall_date date,
reason text,
is_delete bool not null default false,
created_date timestamptz not null default now(),
updated_date timestamptz not null,
created_by int8 null,
updated_by int8 null,
constraint pk_expense_recall primary key (expense_recall_id),
constraint fk_expense_recall_project_id foreign key (project_id) references project(project_id),
constraint fk_expense_recall_site_id foreign key (site_id) references site_contractor(site_contractor_id),
constraint fk_expense_recall_expense_id foreign key (expense_id) references expense(expense_id),
constraint fk_expense_recall_recall_creator_id foreign key (recall_creator_id) references users(user_id),
constraint fk_expense_recall_expense_details_id foreign key (expense_details_id) references expense_details(expense_details_id)
)

alter table purchase_order 
add column purchase_order_details jsonb;

alter table category 
rename column budget to actual_budget;

alter table category 
add column estimated_budget float8;