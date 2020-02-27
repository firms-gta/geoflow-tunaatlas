
CREATE SCHEMA %dimension_name%
  AUTHORIZATION "%db_admin%";

GRANT ALL ON SCHEMA %dimension_name% TO "%db_admin%";
GRANT USAGE ON SCHEMA %dimension_name% TO "%db_read%";
ALTER DEFAULT PRIVILEGES IN SCHEMA %dimension_name% GRANT SELECT ON TABLES TO "%db_read%";


CREATE TABLE %dimension_name%.%dimension_name%
(
  id_%dimension_name% serial NOT NULL,
  codesource_%dimension_name% text,
  tablesource_%dimension_name% text,
  id_metadata integer,
  CONSTRAINT %dimension_name%_pkey PRIMARY KEY (id_%dimension_name%),
  CONSTRAINT %dimension_name%_id_metadata_fkey FOREIGN KEY (id_metadata)
      REFERENCES metadata.metadata (id_metadata) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT %dimension_name%_codesource_%dimension_name%_tablesource_%dimension_name%_key UNIQUE (codesource_%dimension_name%, tablesource_%dimension_name%)
);

ALTER TABLE %dimension_name%.%dimension_name%
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE %dimension_name%.%dimension_name% TO "%db_admin%";

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
      ON UPDATE NO ACTION ON DELETE NO ACTION
);

ALTER TABLE %dimension_name%.%dimension_name%_mapping
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE %dimension_name%.%dimension_name%_mapping TO "%db_admin%";

INSERT INTO %dimension_name%.%dimension_name%(id_%dimension_name%,codesource_%dimension_name%) VALUES (0,'UNK');

INSERT INTO %dimension_name%.%dimension_name%_mapping(%dimension_name%_mapping_id_from,%dimension_name%_mapping_id_to,%dimension_name%_mapping_relation_type) VALUES (0,0,'equal');


CREATE VIEW %dimension_name%.%dimension_name%_labels AS 
 SELECT 0 AS id_%dimension_name%,
    'UNK'::text AS codesource_%dimension_name%,
    NULL::text AS tablesource_%dimension_name%,
    'Unknown'::character varying AS source_label,
    'Inconnu'::character varying AS source_french_label,
    'Desconocido'::character varying AS source_spanish_label;
    

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

COMMENT ON SCHEMA %dimension_name% IS 'Schema containing the %dimension_name% code lists (i.e. reference data) used in the datasets';
COMMENT ON VIEW %dimension_name%.%dimension_name%_mapping_view IS 'View gathering all the mappings (i.e. corresondances) between code lists available for the dimension %dimension_name%';
COMMENT ON VIEW %dimension_name%.%dimension_name%_labels IS 'View gathering the codes and labels of all the code lists available under the schema %dimension_name%';
