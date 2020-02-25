DROP TABLE IF EXISTS "metadata"."metadata_DCMI" CASCADE;

CREATE TABLE "metadata"."metadata_DCMI"
(
  "id_metadata" serial NOT NULL,
  "Identifier" text,
  "Description" text,
  "Title" text,
  "Subject" text,
  "Contact" text,
  "Date" text, 
  "Type" text,
  "SpatialCoverage" text,
  "TemporalCoverage" text,
  "Language" text,
  "Relation" text,
  "Rights" text,
  "Source" text,
  "Provenance" text,
  "Format" text, 
  "Data" text,
   geometry geometry(GEOMETRY,4326),
  CONSTRAINT metadata_pkey PRIMARY KEY ("id_metadata")
) 
WITH (
  OIDS=FALSE
);


ALTER TABLE "metadata"."metadata_DCMI" OWNER TO "%db_admin%";
GRANT SELECT ON TABLE "metadata"."metadata_DCMI" TO "%db_admin%";
GRANT ALL ON TABLE "metadata"."metadata_DCMI" TO "%db_admin%";

CREATE INDEX "metadata_geometry_session_geom_idx" ON "metadata"."metadata_DCMI" USING GIST ("geometry");

COMMENT ON TABLE "metadata"."metadata_DCMI" IS 'Table containing the metadata on all the datasets available in the database using DCMI Metadata Terms (https://www.dublincore.org/specifications/dublin-core/dcmi-terms)';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."id_metadata" IS '"id_metadata" is a Serial (integer) which can be used as another primary key to identify each dataset within the database model';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Identifier" IS '"Identifier" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/identifier): An unambiguous reference to the resource within a given context (eg session_id for input datasets)';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Description" IS '"Description" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/description): An account of the resource. Description may include but is not limited to: an abstract, a table of contents, a graphical representation, or a free-text account of the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Title" IS '"Title" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/title): A name given to the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Subject" IS '"Subject" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/subject): The topic of the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Contact" IS 'customized field includes all contacts as defined by DCMI (http://purl.org/dc/elements/1.1/creator): (Creator,contributor, publisher...)';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Date" IS '"Date" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/date): A point or period of time associated with an event in the lifecycle of the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Type" IS '"Type" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/type): The nature or genre of the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."SpatialCoverage" IS '"SpatialCoverage" metadata element as defined by DCMI (http://purl.org/dc/terms/spatial): Spatial characteristics of the resource (which refines coverage). In this case, we use EWKT Postgis standard to store the geometry of the feature as text (Spatial reference system and related list of coordinates). When the raw geometry is too big we use the "SpatialCoverage" to describe a simplified version of the original geometry.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."TemporalCoverage" IS '"TemporalCoverage" metadata element as defined by DCMI (http://purl.org/dc/terms/temporal): Temporal characteristics of the resource (which refines coverage)';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Language" IS '"Language" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/language): A language of the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Relation" IS '"Relation" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/relation): A related resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Rights" IS '"Rights" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/rights): Information about rights held in and over the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Source" IS '"Source" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/source): A related resource from which the described resource is derived.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Provenance" IS '"Provenance" metadata element as defined by DCMI (http://purl.org/dc/terms/provenance): A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Format" IS '"Format" metadata element as defined by DCMI (http://purl.org/dc/elements/1.1/format): The file format, physical medium, or dimensions of the resource.';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."Data" IS '"Data" metadata element: information about the data described by (above) DCMI metadata elements. Syntactic rules, if applied, are following guidelines of geoflow R package (https://github.com/eblondel/geoflow)';
COMMENT ON COLUMN "metadata"."metadata_DCMI"."geometry" IS '"geometry" metadata element which stores the spatial geometry (SFS) representing the dataset (eg a set of track points collected by the GPS during the session). The geometry can be the binary version of the "SpatialCoverage" metadata element or a more accurate one.';
