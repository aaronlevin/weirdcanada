create or replace function convert_format(int) returns text as $BODY$
begin
  if $1 = 0 then return 'CD';
  elseif $1 = 1 then return '12"';
  elseif $1 = 2 then return '7"';
  elseif $1 = 3 then return '10"';
  elseif $1 = 4 then return 'cassette';
  elseif $1 = 5 then return 'digital';
  elseif $1 = 6 then return 'lathe';
  elseif $1 = 7 then return 'vinyl';
  elseif $1 = 8 then return 'book';
  elseif $1 = 9 then return 't-shirt';
  elseif $1 = 10 then return 'dvd';
  elseif $1 = 11 then return 'other';
  else return 'unknown';
  end if;
end
$BODY$
language plpgsql;
CREATE FUNCTION
