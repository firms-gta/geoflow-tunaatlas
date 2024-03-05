-- View: public.plot_i6i7i8

-- DROP MATERIALIZED VIEW public.plot_i6i7i8;

CREATE MATERIALIZED VIEW public.plot_i6i7i8
TABLESPACE pg_default
AS
SELECT 
	row_number() OVER () AS ogc_fid,
	metadata.identifier AS dataset,
	"time".year,
	species_labels.codesource_species AS species,
	fishing_fleet_labels.codesource_fishing_fleet AS fishing_fleet,
	sum(fact.measurement_value) AS measurement_value,
	count(fact.measurement_value) AS count,
    measurement_unit_labels.codesource_measurement_unit AS unit,
	area.codesource_area,
	cwp_grid.geom,
	fact.id_area AS geom_id

FROM metadata.metadata, fact_tables.catch fact
	LEFT JOIN "time"."time" USING (id_time)
	
	LEFT JOIN area.area USING (id_area)
	LEFT JOIN area.cwp_grid ON cwp_grid.code = area.codesource_area
	
	LEFT JOIN species.species_labels USING (id_species)
	LEFT JOIN species.species_mapping_view ON species_mapping_view.db_idsource = fact.id_species
	LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping_view.db_idtarget
	
	LEFT JOIN fishing_fleet.fishing_fleet_labels USING (id_fishing_fleet)
	LEFT JOIN fishing_fleet.fishing_fleet_mapping_view ON fishing_fleet_mapping_view.db_idsource = fact.id_fishing_fleet
	LEFT JOIN fishing_fleet.fishing_fleet_labels fishing_fleetgroup_label ON fishing_fleetgroup_label.id_fishing_fleet = fishing_fleet_mapping_view.db_idtarget
	
     LEFT JOIN measurement_unit.measurement_unit_labels USING (id_measurement_unit)

WHERE
  metadata.identifier = ANY(ARRAY['global_catch_5deg_1m_firms_level0'::text,'global_catch_5deg_1m_firms_level1'::text,'global_catch_5deg_1m_ird_level2'::text,'global_catch_ird_level2'::text,
  'global_catch_firms_level0'::text, 'global_nominal_catch_firms'::text])
  AND 
  metadata.id_metadata = fact.id_metadata 
  AND 
  cwp_grid.gridtype = '5deg_x_5deg'::text 
  
GROUP BY metadata.identifier, fact.id_area, area.codesource_area, "time".year, species_labels.codesource_species, fishing_fleet_labels.codesource_fishing_fleet, cwp_grid.geom, measurement_unit_labels.codesource_measurement_unit 
WITH DATA;

ALTER TABLE public.plot_i6i7i8
    OWNER TO tunaatlas_u;

GRANT ALL ON TABLE public.plot_i6i7i8 TO tunaatlas_u;
GRANT SELECT ON TABLE public.plot_i6i7i8 TO tunaatlas_inv;
