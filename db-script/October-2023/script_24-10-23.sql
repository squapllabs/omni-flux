create sequence indent_request_code_sequence
start 1
increment 1;

select concat('IND',DATE_PART('year', CURRENT_DATE),'00',nextval('indent_request_code_sequence')::text) as indent_request_code_sequence;

alter table indent_request 
add column indent_request_code varchar(50) ;

CREATE OR REPLACE FUNCTION generate_indent_request_code()
RETURNS VARCHAR(50) AS $$
BEGIN
  RETURN CONCAT(
    'IND',
    DATE_PART('year', CURRENT_DATE),
    '00',
    NEXTVAL('indent_request_code_sequence')::TEXT
  );
END;
$$ LANGUAGE plpgsql;

UPDATE indent_request
SET indent_request_code = generate_indent_request_code()
WHERE indent_request_code IS NULL;

alter table indent_request 
alter column indent_request_code set not null;