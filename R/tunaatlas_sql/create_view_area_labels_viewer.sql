-- Area labels for dataset views in Tuna atlas map viewer
-- These materialized views should be managed once the database area labels are loaded
-- To improve the dataset views, 1deg and 5deg grids are cut by continent.

CREATE INDEX continent_geom_idx ON public.continent USING GIST (the_geom);
CREATE INDEX area_area_labels_geom_idx ON area.area_labels USING GIST (geom);

CREATE MATERIALIZED VIEW area.erased_area_labels AS
SELECT * FROM area.area_labels WHERE tablesource_area = 'areas_tuna_rfmos_task1';

CREATE MATERIALIZED VIEW area.grid_area_labels as 
SELECT * FROM area.area_labels WHERE tablesource_area = 'cwp_grid_erased';

CREATE MATERIALIZED VIEW area.grid_5deg_area_labels as 
SELECT * FROM area.grid_area_labels WHERE codesource_area like '6%';
CREATE INDEX grid_area_5deg_labels_id_area_idx  ON area.grid_5deg_area_labels (id_area);
CREATE INDEX grid_area_5deg_labels_codesource_area_idx  ON area.grid_5deg_area_labels (codesource_area);
CREATE INDEX grid_area_5deg_labels_codesource_area_geom_idx ON area.grid_5deg_area_labels USING GIST (geom);


CREATE MATERIALIZED VIEW area.grid_1deg_area_labels as
SELECT * FROM area.grid_area_labels WHERE codesource_area like '5%';
CREATE INDEX grid_area_1deg_labels_id_area_idx  ON area.grid_1deg_area_labels (id_area);
CREATE INDEX grid_area_1deg_labels_codesource_area_idx  ON area.grid_1deg_area_labels (codesource_area);
CREATE INDEX grid_area_1deg_labels_codesource_area_geom_idx ON area.grid_1deg_area_labels USING GIST (geom);