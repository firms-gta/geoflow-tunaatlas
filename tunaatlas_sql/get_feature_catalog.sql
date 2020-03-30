SELECT 
       CASE WHEN table_type='v' THEN 'view'
            WHEN table_type='m' THEN 'materialized view'
            WHEN table_type='r' THEN 'table'
            ELSE 'autres'
       END AS "TableType",
       schema_name AS "SchemaName", 
       view_name AS "FeatureType", 
       toto.column_name AS "MemberCode", 
       CASE WHEN toto.column_name='catch' THEN 'variable'
            WHEN toto.column_name='effort' THEN 'variable'
            ELSE 'attribute'
       END AS "MemberType",
       description AS "Definition",
       data_type AS attribute_valueType,
       max_length,
       is_nullable 

	FROM

  (SELECT  
	n.nspname, 
	c.relname, 
	a.attname As column_name, 
	d.description,
	c.relkind as table_type
   FROM 
	pg_class As c
	INNER JOIN pg_attribute As a ON c.oid = a.attrelid
	LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
	LEFT JOIN pg_tablespace t ON t.oid = c.reltablespace
	LEFT JOIN pg_description As d ON (d.objoid = c.oid AND d.objsubid = a.attnum)
   WHERE  
	c.relkind IN('r') 
	AND  
	n.nspname NOT IN  ('information_schema', 'pg_catalog', 'topology','public')
   ORDER BY 
	n.nspname, c.relname, a.attname
	) AS toto 

JOIN 
	(SELECT
		t.table_schema as schema_name,
		t.table_name as view_name,
		c.column_name,
		c.data_type,
		case when c.character_maximum_length is not null
		    then c.character_maximum_length
		    else c.numeric_precision end as max_length,
		is_nullable
	   FROM 
		information_schema.tables t
		left join information_schema.columns c 
		on t.table_schema = c.table_schema 
		and t.table_name = c.table_name
	   WHERE   
		t.table_schema not in ('information_schema', 'pg_catalog', 'topology') 
	   ORDER BY 
		schema_name,
		view_name 
	) AS tutu 

ON	 

tutu.schema_name=toto.nspname AND tutu.view_name=toto.relname AND tutu.column_name=toto.column_name