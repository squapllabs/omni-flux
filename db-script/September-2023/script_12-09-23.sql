alter table store 
ALTER COLUMN created_by TYPE integer USING created_by::integer;

alter table store 
ALTER COLUMN updated_by TYPE integer USING updated_by::integer;