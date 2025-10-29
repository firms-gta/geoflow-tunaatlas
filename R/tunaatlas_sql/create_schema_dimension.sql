DROP SCHEMA IF EXISTS %dimension_name% CASCADE;

CREATE SCHEMA %dimension_name%
  AUTHORIZATION "%db_admin%";

GRANT ALL ON SCHEMA %dimension_name% TO "%db_admin%";
GRANT USAGE ON SCHEMA %dimension_name% TO "%db_read%";
ALTER DEFAULT PRIVILEGES IN SCHEMA %dimension_name% GRANT SELECT ON TABLES TO "%db_read%";
COMMENT ON SCHEMA %dimension_name% IS 'Schema containing the %dimension_name% code lists (i.e. reference data) used in the datasets';



CREATE TABLE %dimension_name%.%dimension_name%
(
  id_%dimension_name% serial NOT NULL,
  codesource_%dimension_name% text,
  tablesource_%dimension_name% text,
  id_metadata integer,
  CONSTRAINT %dimension_name%_pkey PRIMARY KEY (id_%dimension_name%),
  CONSTRAINT %dimension_name%_id_metadata_fkey FOREIGN KEY (id_metadata)
      REFERENCES metadata.metadata (id_metadata) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE,
  CONSTRAINT %dimension_name%_codesource_%dimension_name%_tablesource_%dimension_name%_key UNIQUE (codesource_%dimension_name%, tablesource_%dimension_name%)
);

ALTER TABLE %dimension_name%.%dimension_name%
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE %dimension_name%.%dimension_name% TO "%db_admin%";
COMMENT ON TABLE  %dimension_name%.%dimension_name%  IS '"%dimension_name%.%dimension_name%" is a dimension of the data warehouse: a list of codes which gives the context of the values stored in the fact table. Paul: This table exists for each dimension (1 table by dimension available in the DW). It gathers all the primary keys (i.e. codes) of all the reference data of the dimension. It enables to make the link between the reference data and the fact tables that use these reference data (see DW documentation for more details).';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%"."id_%dimension_name%" IS '"id_%dimension_name%"  column identifies (primary key) the %dimension_name% with a numeric code (integer). Paul: Primary key. Technical numerical identifier for each code used in the reference table of the dimension.';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%"."codesource_%dimension_name%" IS '"codesource_%dimension_name%" indicates what is the label in the dataset where this code comes from. Paul: Conceptually references each reference table of the dimension (the reference is not technically implemented in the physical model). Code of the reference table.';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%"."tablesource_%dimension_name%" IS '"tablesource_%dimension_name%" gives the name of the dataset from which dataset this code comes. Paul: Identifier of the table that contains the code.';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%"."id_metadata" IS '"id_metadata" indicates from which dataset this code comes from by pointing the identifier (foreign key) of the related metadata in the metadata table. Paul: Foreign key - References the table metadata. Metadata of the reference table that contains the code.';



CREATE TABLE %dimension_name%.%dimension_name%_mapping
(
  %dimension_name%_mapping_id_from integer NOT NULL,
  %dimension_name%_mapping_id_to integer NOT NULL,
  %dimension_name%_mapping_relation_type character varying(20) NOT NULL,
  id_metadata integer,
  CONSTRAINT %dimension_name%_mapping_pkey PRIMARY KEY (%dimension_name%_mapping_id_from, %dimension_name%_mapping_id_to, %dimension_name%_mapping_relation_type),
  CONSTRAINT %dimension_name%_mapping_%dimension_name%_mapping_id_from_fkey FOREIGN KEY (%dimension_name%_mapping_id_from)
      REFERENCES %dimension_name%.%dimension_name% (id_%dimension_name%) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT %dimension_name%_mapping_%dimension_name%_mapping_id_to_fkey FOREIGN KEY (%dimension_name%_mapping_id_to)
      REFERENCES %dimension_name%.%dimension_name% (id_%dimension_name%) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT %dimension_name%_mapping_id_metadata_fkey FOREIGN KEY (id_metadata)
      REFERENCES metadata.metadata (id_metadata) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE CASCADE
);

ALTER TABLE %dimension_name%.%dimension_name%_mapping
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE "%dimension_name%"."%dimension_name%_mapping" TO "%db_admin%";
COMMENT ON TABLE  "%dimension_name%"."%dimension_name%_mapping"  IS '"%dimension_name%"."%dimension_name%_mapping" enables to swicth from a codification system to another. For example, this table is required to generate global datasets from regional datasets which use specific code lists. Paul: This table exists for each dimension (1 table by dimension available in the DW). It stores the eventual mappings (i.e. correspondences) between code lists belonging to the dimension.';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%_mapping"."%dimension_name%_mapping_id_from" IS '"%dimension_name%_mapping_id_from"  is the identifier (foreign key) of the code which is translated by this mapping relationship into another codification system. Paul: Foreign key - References the table <dimension>. id (from the table <dimension>) of the source dataset.';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%_mapping"."%dimension_name%_mapping_id_to" IS '"%dimension_name%_mapping_id_to" gives the identifier (foreign key) of the code which results from the mapping. Paul: Foreign key - References the table %dimension_name% id (from the table <dimension>) of the target dataset.';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%_mapping"."%dimension_name%_mapping_relation_type" IS '"%dimension_name%_mapping_relation_type" specifies the kind / semantic of the relationship: same as, generalizes.. Paul : Type of relation that exists between the source code and the target code (e.g. is strictly equivalent to, is part of, etc.).';
COMMENT ON COLUMN "%dimension_name%"."%dimension_name%_mapping"."id_metadata" IS '"id_metadata"...Paul : Foreign key - References the table metadata. Metadata of the mapping.';


INSERT INTO %dimension_name%.%dimension_name%(id_%dimension_name%,codesource_%dimension_name%) VALUES (0,'UNK');
INSERT INTO %dimension_name%.%dimension_name%_mapping(%dimension_name%_mapping_id_from,%dimension_name%_mapping_id_to,%dimension_name%_mapping_relation_type) VALUES (0,0,'equal');


CREATE VIEW %dimension_name%.%dimension_name%_labels AS 
 SELECT 0 AS id_%dimension_name%,
    'UNK'::text AS codesource_%dimension_name%,
    NULL::text AS tablesource_%dimension_name%,
    'Unknown'::character varying AS source_label,
    'Inconnu'::character varying AS source_french_label,
    'Desconocido'::character varying AS source_spanish_label;
    
    
COMMENT ON VIEW %dimension_name%.%dimension_name%_labels IS 'View gathering the codes and labels of all the code lists available under the schema %dimension_name%';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_labels.id_%dimension_name% IS '"id_%dimension_name%" name of the dimension';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_labels.codesource_%dimension_name% IS '"codesource_%dimension_name%" the label of the code in the dataset where this code comes from';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_labels.tablesource_%dimension_name% IS '"tablesource_%dimension_name%" the name of the dataset from which dataset this code comes';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_labels.source_label IS '"source_label" the label of the code in the dataset where this code comes from';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_labels.source_french_label IS '"source_french_label" the french label of the code in the dataset where this code comes from';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_labels.source_spanish_label IS '"source_spanish_label" the spanish label of the code in the dataset where this code comes from';
    

CREATE VIEW %dimension_name%.%dimension_name%_mapping_view AS 
SELECT 
sub1.db_idsource,
sub2.db_idtarget,
sub1.codesource as src_code,
sub2.codetarget as trg_code,
sub1.db_tablesource as src_codingsystem,
sub2.db_tabletarget as trg_codingsystem,
sub2.relation_type
FROM
( SELECT %dimension_name%.id_%dimension_name% AS db_idsource,
%dimension_name%.codesource_%dimension_name% AS codesource,
%dimension_name%.tablesource_%dimension_name% AS db_tablesource
FROM %dimension_name%.%dimension_name%
JOIN metadata.metadata ON metadata.id_metadata = %dimension_name%.id_metadata
) sub1
LEFT JOIN ( SELECT %dimension_name%_mapping.%dimension_name%_mapping_id_from as db_idsource,
 %dimension_name%_mapping.%dimension_name%_mapping_id_to AS db_idtarget,
%dimension_name%_mapping.%dimension_name%_mapping_relation_type as relation_type, 
%dimension_name%.codesource_%dimension_name% AS codetarget,
%dimension_name%.tablesource_%dimension_name% AS db_tabletarget
FROM %dimension_name%.%dimension_name%_mapping
JOIN %dimension_name%.%dimension_name% ON %dimension_name%.id_%dimension_name% = %dimension_name%_mapping.%dimension_name%_mapping_id_to
JOIN metadata.metadata ON metadata.id_metadata = %dimension_name%.id_metadata
) sub2 ON sub1.db_idsource = sub2.db_idsource
ORDER BY sub1.db_tablesource,sub1.codesource,sub2.db_tabletarget;

COMMENT ON VIEW %dimension_name%.%dimension_name%_mapping_view IS 'View gathering all the mappings (i.e. corresondances) between code lists available for the dimension %dimension_name%';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_mapping_view.db_idsource IS '"db_idsource" name of the dimension';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_mapping_view.db_idtarget IS '"db_idtarget" identifier of the code resulting from the mapping';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_mapping_view.src_code IS '"src_code" the label of the code in the dataset where this code comes from';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_mapping_view.trg_code IS '"trg_code" the label of the code which results from the mapping';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_mapping_view.src_codingsystem IS '"src_codingsystem" the name of the dataset from which dataset this code comes';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_mapping_view.trg_codingsystem IS '"trg_codingsystem" the name of the standard codelist which is the ouput of the mapping';
COMMENT ON COLUMN %dimension_name%.%dimension_name%_mapping_view.relation_type IS '"relation_type" the kind of relationship between these two codes: same as, generalizes..';
