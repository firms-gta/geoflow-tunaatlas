-- Grid area labels for dataset views in Tuna atlas map viewer
-- These materialized views should be managed once the database area labels are loaded
-- To improve the dataset views, 1deg and 5deg grids are cut by continent.

CREATE MATERIALIZED VIEW area.grid_area_labels AS
SELECT * FROM area.area_labels WHERE tablesource_area = 'areas_tuna_rfmos_task2';
CREATE INDEX grid_area_labels_id_area_idx  ON area.grid_area_labels  (id_area);
CREATE INDEX grid_area_labels_codesource_area_idx  ON area.grid_area_labels (codesource_area);

CREATE MATERIALIZED VIEW area.grid_5deg_area_labels as 
SELECT ar.id_area, ar.codesource_area, ar.tablesource_area, ar.source_label, st_difference(ar.geom, cont.geometry) as geom FROM area.grid_area_labels as ar, public.continent as cont WHERE ar.codesource_area like '6%' and st_intersects(ar.geom, cont.geometry);
CREATE INDEX grid_area_5deg_labels_id_area_idx  ON area.grid_5deg_area_labels  (id_area);
CREATE INDEX grid_area_5deg_labels_codesource_area_idx  ON area.grid_5deg_area_labels (codesource_area);

CREATE MATERIALIZED VIEW area.grid_1deg_area_labels as
SELECT ar.id_area, ar.codesource_area, ar.tablesource_area, ar.source_label, st_difference(ar.geom, cont.geometry) as geom FROM area.grid_area_labels as ar, public.continent as cont WHERE ar.codesource_area like '5%' and st_intersects(ar.geom, cont.geometry);
CREATE INDEX grid_area_1deg_labels_id_area_idx  ON area.grid_1deg_area_labels  (id_area);
CREATE INDEX grid_area_1deg_labels_codesource_area_idx  ON area.grid_1deg_area_labels (codesource_area);