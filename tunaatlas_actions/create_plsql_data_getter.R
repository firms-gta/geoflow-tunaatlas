create_plsql_data_getter <- function(entity, config, options){
	
	if(!require(readr)){
		install.packages("readr")
		require(readr)
	}
	
	con <- config$software$output$dbi

	#set information required for (meta)data services	
	df_codelists <- as.data.frame(readr::read_csv(entity$resources$codelists, guess_max=0))
	dimensions <- c(df_codelists[df_codelists$dimension != "area", "dimension"], "time_start", "time_end", "year", "quarter", "month", "aggregation_method")

	fact <- unlist(strsplit(entity$data$uploadSource[[1]], "\\."))[2]
	sql_params <- paste0("schema_name varchar, pid varchar,", paste0(paste0("input_", dimensions, " varchar"), collapse = ","), ",geom_table varchar")
	sql_drop <- sprintf("DROP FUNCTION public.get_fact_dataset_%s(%s)", fact, paste0(rep("varchar", length(dimensions)+2),collapse=","))
	sql_create <- sprintf("CREATE OR REPLACE FUNCTION public.get_fact_dataset_by_%s(%s) \n", fact, sql_params)
	
	sql_table_columns <- c(dimensions[dimensions != "aggregation_method"], "value", "geographic_identifier", "geom")
	sql_table_columns <- paste0(sapply(sql_table_columns, function(x){
		type <- switch(x,
			"time_start" = "timestamp",
			"time_end" = "timestamp",
			"value" = "numeric",
			"geom" = "geometry",
			"year" =  "integer",
			"quarter" = "integer",
			"month" = "integer",
			"text"
		)
		return(paste(x, type))
	}), collapse=",")
	sql_table_out <- sprintf("RETURNS TABLE(%s) AS $$ \n", sql_table_columns)
	sql_create <- paste0(sql_create, sql_table_out)
	
	#declare block
	sql_create <- paste0(sql_create,
		"DECLARE
			count_month integer := 12;  
			count_quarter integer := 4;
			count_year integer := 1;
			count_yeartime integer := 0;
			query varchar := '';
		")
	#begin block
	
	#filters
	sql_query_filters <- sapply(dimensions[dimensions != "aggregation_method"], function(x){
		cast <- ""
		if(x %in% c("month", "quarter", "year")) cast <- "::numeric"
		sql_filter <- sprintf("%s IN( select regexp_split_to_table(regexp_replace('''||input_%s||''','' '', ''+'', ''g''),E''\\\\+'')%s )", x, x, cast)
		
		if(x == "time_start") sql_filter <- sprintf("%s >= '''||input_%s||'''", x, x)
		if(x == "time_end") sql_filter <- sprintf("%s <= '''||input_%s||'''", x, x)
		return(sql_filter)
	})	
	sql_query_filters <- paste0(sql_query_filters, collapse = " AND ")
	
	#sql query for raw data
	sql_query_raw_select_columns <- paste0(sapply(dimensions[dimensions != "aggregation_method"], function(x){
		out <- paste0("dataset.", x)
		#if(x %in% c("year", "quarter", "month")) out <- paste0("CAST(", out, " AS text)")
		return(out)
	}), collapse=", ")
	
	sql_query_raw <- paste0("SELECT ", sql_query_raw_select_columns, ", dataset.value, tab_geom.codesource_area as geographic_identifier, tab_geom.geom as the_geom FROM '||schema_name||'.' || pid || ' dataset LEFT OUTER JOIN '||geom_table||' tab_geom USING (id_area) \n")
	sql_query_raw <- paste0(sql_query_raw, "WHERE \n")	
	sql_query_raw <- paste0(sql_query_raw, sql_query_filters, ";")
	
	
	#sql query for aggregate data
	sql_query_agg_select_columns <- paste0(sapply(dimensions[dimensions != "aggregation_method"], function(x){
		type_cast <- "text"
		if(x %in% c("time_start", "time_end"))  type_cast <- "timestamp"
		if(x %in% c("year", "quarter", "month")) type_cast <- "integer"
		#out <- sprintf("CAST('''||input_%s||''' AS %s)", x, type_cast)
		out <- sprintf("CAST(NULL AS %s)", type_cast)
		return(out)
	}), collapse=", ")
	
	sql_query_agg <- paste0("SELECT ", sql_query_agg_select_columns, ", CAST(dataset.value AS numeric), tab_geom.codesource_area as geographic_identifier,tab_geom.geom as the_geom from ( SELECT CASE '''|| input_aggregation_method ||''' WHEN ''sum'' THEN sum(value) WHEN ''avg_by_year'' THEN sum(value)/'|| count_year ||' WHEN ''avg_by_quarter'' THEN sum(value)/'|| count_year * count_quarter ||' WHEN ''avg_by_month'' THEN sum(value)/'|| count_year * count_month ||' END as value, id_area FROM '|| schema_name ||'.'|| pid ||' WHERE ", sql_query_filters," GROUP BY id_area) dataset LEFT OUTER JOIN '||geom_table||' tab_geom USING (id_area) ;")
	
	sql_create <- paste(sql_create,
		"BEGIN

			IF input_aggregation_method = 'none' THEN
				RAISE notice 'Running query without aggregation';
				query = '",sql_query_raw,"';
				RAISE notice 'SQL: %', query;
				RETURN QUERY EXECUTE query; 
			ELSE
				RAISE notice 'Running query with aggregation method: %', input_aggregation_method;
				SELECT INTO count_year COUNT(*) FROM regexp_split_to_table(regexp_replace(input_year,' ', '+', 'g'),E'\\\\+');
				SELECT INTO count_yeartime (DATE_PART('year', input_time_end::date) - DATE_PART('year', input_time_start::date) + 1);
				IF count_yeartime < count_year AND count_yeartime > 0 THEN
					count_year = count_yeartime;
				END IF;
				IF input_aggregation_method = 'avg_by_month' THEN
					SELECT INTO count_month COUNT(*) FROM regexp_split_to_table(regexp_replace(input_month,' ', '+', 'g'),E'\\\\+');
					RAISE notice 'Average on % months', count_year * count_month;
				ELSIF input_aggregation_method = 'avg_by_quarter' THEN
					SELECT INTO count_quarter COUNT(*) FROM regexp_split_to_table(regexp_replace(input_quarter,' ', '+', 'g'),E'\\\\+');
					RAISE notice 'Average on % quarters', count_year * count_quarter;
				ELSIF input_aggregation_method = 'avg_by_year' THEN
					RAISE notice 'Average on % years', count_year;
				END IF;
				query = '",sql_query_agg,"';
				RAISE notice 'SQL: %', query;
				RETURN QUERY EXECUTE query;
			END IF;
		END; $$ 
	LANGUAGE plpgsql;")
	config$logger.info("Create or Replace PL/PSQL function to get dataset")
	config$logger.info(paste("\n", sql_create))
	dbSendQuery(con, sql_create)
}