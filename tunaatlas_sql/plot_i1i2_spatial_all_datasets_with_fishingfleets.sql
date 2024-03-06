-- View: public.plot_i1i2_spatial_all_datasets_with_fishingfleets

-- DROP MATERIALIZED VIEW public.plot_i1i2_spatial_all_datasets_with_fishingfleets;

CREATE MATERIALIZED VIEW public.plot_i1i2_spatial_all_datasets_with_fishingfleets
TABLESPACE pg_default
AS



SELECT metadata.identifier AS dataset,
    source_labels.codesource_source AS ocean,
    "time".year,
    geargoup_label.codesource_gear_type AS gear_group,
    fishing_fleet_labels.source_label AS fishing_fleet,
    species_labels.codesource_species AS species,
    sum(tab.measurement_value) AS value,
    measurement_unit_labels.codesource_measurement_unit AS unit,
    st_area(area_labels.geom) AS area,
    area_labels.geom
   FROM metadata.metadata,
    fact_tables.catch tab
     LEFT JOIN measurement_unit.measurement_unit_labels USING (id_measurement_unit)
     LEFT JOIN source.source_labels USING (id_source)
     LEFT JOIN gear_type.gear_type_labels USING (id_gear_type)
     LEFT JOIN gear_type.gear_type_mapping ON gear_type_mapping.gear_type_mapping_id_from = tab.id_gear_type
     LEFT JOIN gear_type.gear_type_labels geargoup_label ON geargoup_label.id_gear_type = gear_type_mapping.gear_type_mapping_id_to
     LEFT JOIN fishing_fleet.fishing_fleet_labels USING (id_fishing_fleet)
     LEFT JOIN area.area USING (id_area)
     LEFT JOIN area.area_labels USING (id_area)
     LEFT JOIN "time"."time" USING (id_time)
     LEFT JOIN species.species_labels USING (id_species)
     LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
     LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to
  WHERE (metadata.identifier = ANY (ARRAY['global_nominal_catch_firms_level0'::text, 'global_catch_firms_level0'::text, 'global_catch_5deg_1m_firms_level0'::text, 'global_catch_1deg_1m_ps_bb_firms_level0'::text, 'global_catch_1deg_1m_ps_bb_ird_level1'::text, 'global_catch_1deg_1m_ps_bb_ird_level2'::text, 'global_catch_5deg_1m_ird_level1'::text, 'global_catch_5deg_1m_ird_level2'::text, 'global_catch_ird_level1'::text, 'global_catch_ird_level2'::text])) AND metadata.id_metadata = tab.id_metadata
  GROUP BY metadata.identifier, source_labels.codesource_source, geargoup_label.codesource_gear_type, fishing_fleet_labels.source_label, "time".year, species_labels.codesource_species, measurement_unit_labels.codesource_measurement_unit, area_labels.geom 
  
WITH DATA;

ALTER TABLE public.plot_i1i2_spatial_all_datasets_with_fishingfleets
    OWNER TO tunaatlas_u;

GRANT ALL ON TABLE public.plot_i1i2_spatial_all_datasets_with_fishingfleets TO tunaatlas_u;
GRANT SELECT ON TABLE public.plot_i1i2_spatial_all_datasets_with_fishingfleets TO tunaatlas_inv;
