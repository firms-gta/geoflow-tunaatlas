DROP SCHEMA IF EXISTS size_class CASCADE;
CREATE SCHEMA size_class
  AUTHORIZATION "%db_admin%";

GRANT ALL ON SCHEMA size_class TO "%db_admin%";
GRANT USAGE ON SCHEMA size_class TO "%db_read%";
ALTER DEFAULT PRIVILEGES IN SCHEMA size_class GRANT SELECT ON TABLES TO "%db_read%";


CREATE TABLE size_class.size_class
(
  id_size_class serial NOT NULL,
  size_min real,
  size_step real,
  size_max real,
  size_avg real,
  CONSTRAINT size_class_pkey PRIMARY KEY (id_size_class)
);
ALTER TABLE size_class.size_class
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE size_class.size_class TO "%db_admin%";
COMMENT ON TABLE size_class.size_class IS '"size_class.size_class" table stores the characterstics of the different size class used in the database which are lenght intervalls (fork length), minimal value included, maximal value excluded' ;
COMMENT ON COLUMN size_class.size_class.id_size_class IS '"id_size_class" identifier of this size class';
COMMENT ON COLUMN size_class.size_class.size_min IS '"size_min" the minimal size of this intervall';
COMMENT ON COLUMN size_class.size_class.size_step IS '"size_step" the width of the intervall which can be different (1cm, 2cm..)';
COMMENT ON COLUMN size_class.size_class.size_max IS '"size_max" the maximal size of this intervall';
COMMENT ON COLUMN size_class.size_class.size_avg IS '"size_avg" average size';
    

CREATE OR REPLACE FUNCTION size_class.size_class_calc_size_avg()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.size_avg=(NEW.size_min+NEW.size_min+NEW.size_step)/2;
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION size_class.size_class_calc_size_avg()
  OWNER TO "%db_admin%";
GRANT EXECUTE ON FUNCTION size_class.size_class_calc_size_avg() TO "%db_admin%";

CREATE OR REPLACE FUNCTION size_class.size_class_calc_size_max()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.size_max=NEW.size_min+NEW.size_step;
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION size_class.size_class_calc_size_max()
  OWNER TO "%db_admin%";
GRANT EXECUTE ON FUNCTION size_class.size_class_calc_size_max() TO "%db_admin%";
  

CREATE TRIGGER trigg_size_class_calc_size_avg
  BEFORE INSERT OR UPDATE
  ON size_class.size_class
  FOR EACH ROW
  EXECUTE PROCEDURE size_class.size_class_calc_size_avg();


CREATE TRIGGER trigg_size_class_calc_size_max
  BEFORE INSERT OR UPDATE
  ON size_class.size_class
  FOR EACH ROW
  EXECUTE PROCEDURE size_class.size_class_calc_size_max();
  
  INSERT INTO size_class.size_class(size_min,size_step) VALUES (1,0);
