CREATE TABLE area.area_wkt
(
  code text NOT NULL,
  geom geometry(Polygon,4326),
  CONSTRAINT code_wkt_pkey PRIMARY KEY (code)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE area.area_wkt
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE area.area_wkt TO "%db_admin%";

CREATE OR REPLACE FUNCTION area.func_add_new_record_in_link_table_area_wkt()
  RETURNS trigger AS
$BODY$ BEGIN INSERT INTO area.area ( codesource_area,tablesource_area) VALUES (NEW.code,'area_wkt') ; RETURN NEW; END; $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION area.func_add_new_record_in_link_table_area_wkt()
  OWNER TO "%db_admin%";

CREATE TRIGGER trig_add_new_record_in_link_table_area_wkt
  BEFORE INSERT
  ON area.area_wkt
  FOR EACH ROW
  EXECUTE PROCEDURE area.func_add_new_record_in_link_table_area_wkt();

CREATE OR REPLACE FUNCTION area.area_wkt_calc_geom()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.geom=ST_GeomFromText(NEW.code, 4326);
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION area.area_wkt_calc_geom()
  OWNER TO "%db_admin%";
GRANT EXECUTE ON FUNCTION area.area_wkt_calc_geom() TO "%db_admin%";


CREATE TRIGGER trigg_area_wkt_calc_geom
  BEFORE INSERT OR UPDATE
  ON area.area_wkt
  FOR EACH ROW
  EXECUTE PROCEDURE area.area_wkt_calc_geom();


INSERT INTO area.area_wkt(code) VALUES ('POLYGON((-18.5 6.5,-18.5 7,-18 7,-18 6.5,-18.5 6.5))');
