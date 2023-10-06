-- Expense - Alter Script
alter table expense 
add column total_amount float8;

-- Expense Details - Alter Script
alter table expense_details 
add column description text;

alter table expense_details 
add column quantity int4;

alter table expense_details 
add column unit_value int4;

--Sub Category - Alter Script

alter table sub_category 
add column parent_sub_category_id int4;

alter table sub_category 
add constraint fk_sub_category_parent_sub_category_id foreign key (parent_sub_category_id) references sub_category(sub_category_id);

alter table sub_category 
rename column budget to actual_budget;

alter table sub_category 
add column estimated_budget float8;

alter table sub_category 
add column uom_id int4;

alter table sub_category 
add constraint fk_sub_category_uom_id foreign key (uom_id) references uom(uom_id);

alter table sub_category 
add column quantity int4;