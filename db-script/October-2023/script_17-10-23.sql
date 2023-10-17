alter table sub_category 
alter column name drop not null;

alter table expense 
add column user_id int4;

alter table expense 
add constraint fk_expense_user_id foreign key (user_id) references users(user_id);