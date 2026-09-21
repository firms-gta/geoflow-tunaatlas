-- Area labels for dataset views in Tuna atlas map viewer
-- These materialized views should be managed once the database area labels are loaded
-- To improve the dataset views, 1deg and 5deg grids are cut by continent. --> made with DO $$ BEGIN because multiple query

DO $$
BEGIN
    DROP MATERIALIZED VIEW IF EXISTS area.grid_1deg_area_labels;
    DROP MATERIALIZED VIEW IF EXISTS area.grid_5deg_area_labels;
    DROP MATERIALIZED VIEW IF EXISTS area.grid_area_labels;

    -- 1. Index
    CREATE INDEX IF NOT EXISTS area_area_labels_geom_idx 
        ON area.area_labels USING GIST (geom);

    -- 2. Materialized Views
    CREATE MATERIALIZED VIEW IF NOT EXISTS area.erased_area_labels AS
        SELECT * FROM area.area_labels WHERE tablesource_area = 'areas_tuna_rfmos_task1';

    -- code column cast to text: required for join compatibility with
    -- get_fact_dataset_catch(), whose geographic_identifier is text.
    -- Without this cast, "code" would inherit the double precision type
    -- of codesource_area (as numeric-looking codes), breaking every join.
    CREATE MATERIALIZED VIEW IF NOT EXISTS area.grid_area_labels AS 
        SELECT 
            id_area,
            codesource_area,
            codesource_area::text AS code,
            tablesource_area,
            source_label,
            source_french_label,
            source_spanish_label,
            geom
        FROM area.area_labels 
        WHERE tablesource_area = 'cwp_grid_erased';

    CREATE INDEX IF NOT EXISTS grid_area_labels_code_idx 
        ON area.grid_area_labels (code);

    -- 3. 5deg View + Index
    CREATE MATERIALIZED VIEW IF NOT EXISTS area.grid_5deg_area_labels AS 
        SELECT * FROM area.grid_area_labels WHERE codesource_area LIKE '6%';

    CREATE INDEX IF NOT EXISTS grid_area_5deg_labels_id_area_idx 
        ON area.grid_5deg_area_labels (id_area);
    CREATE INDEX IF NOT EXISTS grid_area_5deg_labels_codesource_area_idx 
        ON area.grid_5deg_area_labels (codesource_area);
    CREATE INDEX IF NOT EXISTS grid_area_5deg_labels_codesource_area_geom_idx 
        ON area.grid_5deg_area_labels USING GIST (geom);

    -- 4. 1deg View + Index
    CREATE MATERIALIZED VIEW IF NOT EXISTS area.grid_1deg_area_labels AS
        SELECT * FROM area.grid_area_labels WHERE codesource_area LIKE '5%';

    CREATE INDEX IF NOT EXISTS grid_area_1deg_labels_id_area_idx 
        ON area.grid_1deg_area_labels (id_area);
    CREATE INDEX IF NOT EXISTS grid_area_1deg_labels_codesource_area_idx 
        ON area.grid_1deg_area_labels (codesource_area);
    CREATE INDEX IF NOT EXISTS grid_area_1deg_labels_codesource_area_geom_idx 
        ON area.grid_1deg_area_labels USING GIST (geom);

END $$;