COPY (
  WITH cc AS (
    SELECT DISTINCT c.id
                  , c.album
                  , c.sku
                  , a.artists
                  , a.title
                  , c.quantity
    FROM consigneditem AS c
    LEFT OUTER JOIN v_album_full_title AS a ON a.album = c.album
  )
  SELECT cc.id AS "consigned item id"
       , cc.album AS "album id"
       , cc.artists AS "artists"
       , cc.title AS "album"
       , cc.sku AS "sku"
       , cc.quantity AS "quantity consigned"
       , SUM(s.quantity) AS "quantity sold"
       , SUM(s.amount) AS "amount"
       , SUM(s.refund) AS "refunded"
       , cc.quantity - SUM(s.quantity) AS "quantity remaining"
       , string_agg(s.refundnote, '|') AS "(debug): refund notes"
       , array_agg(s.orderid) AS "(debug): shopify orders"
       , array_agg(s.lineitemid) AS "(debug): shopify lineitemids"
       , COUNT(*) AS "(debug): number of sales"
  FROM cc
  LEFT OUTER JOIN sale AS s ON cc.id = s.consigneditem
  GROUP BY 1, 2, 3, 4, 5, 6
) TO '/tmp/sales_rollup.tsv' DELIMITER '	' CSV HEADER
