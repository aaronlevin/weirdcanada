copy (

  select string_agg(artist.name, ' // ') as "Artist(s)", a.title as "Title", string_agg(distinct p.name, ' // ') as "Label", convert_format(a.format::int) as "Format", a.weirdcanadaurl as "Weird Canada URL", min(ci.customercost + ci.markup) as "Regular", min(ci.wholesalecost + ci.markup) as "Wholesale", min(ci.markup) as "(markup)" from consigneditem as ci inner join album as a on ci.album = a.id inner join artistsalbums as aa on aa.album = a.id inner join artist as artist on artist.id = aa.artist inner join publishersalbums as pa on pa.album = a.id inner join publisher as p on p.id = pa.publisher group by a.title, a.format, a.weirdcanadaurl
) to '/tmp/test.csv' with csv;
