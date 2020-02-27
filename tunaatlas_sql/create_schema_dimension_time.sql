
CREATE SCHEMA "time"
  AUTHORIZATION "%db_admin%";

GRANT ALL ON SCHEMA "time" TO "%db_admin%";
GRANT USAGE ON SCHEMA "time" TO "%db_read%";
ALTER DEFAULT PRIVILEGES IN SCHEMA "time" GRANT SELECT ON TABLES TO "%db_read%";

CREATE TABLE "time"."time"
(
  id_time serial NOT NULL,
  time_start timestamp without time zone,
  time_end timestamp without time zone,
  time_period interval,
  month integer,
  quarter integer,
  semester integer,
  year integer,
  decade integer,
  month_name character varying(20),
  CONSTRAINT time_pkey PRIMARY KEY (id_time),
  CONSTRAINT time_unique_time_start_time_end UNIQUE (time_start, time_end)
);


ALTER TABLE "time"."time"
  OWNER TO "%db_admin%";
GRANT ALL ON TABLE "time"."time" TO "%db_admin%";



CREATE OR REPLACE FUNCTION time.time_calc_decade()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.decade=extract(decade from NEW.time_start)*10;
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION time.time_calc_decade()
  OWNER TO "%db_admin%";
GRANT EXECUTE ON FUNCTION time.time_calc_decade() TO "%db_admin%";

CREATE OR REPLACE FUNCTION time.time_calc_month()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.month=extract(month from NEW.time_start);
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION time.time_calc_month()
  OWNER TO "%db_admin%";

  CREATE OR REPLACE FUNCTION time.time_calc_month_name()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.month_name=to_char(NEW.time_start::timestamp , 'Month');
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION time.time_calc_month_name()
  OWNER TO "%db_admin%";

CREATE OR REPLACE FUNCTION time.time_calc_quarter()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.quarter=extract(quarter from NEW.time_start);
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION time.time_calc_quarter()
  OWNER TO "%db_admin%";

  CREATE OR REPLACE FUNCTION time.time_calc_semester()
  RETURNS trigger AS
$BODY$
 BEGIN

  IF extract(month from NEW.time_start)<7 THEN
            NEW.semester=1;
        END IF;
         IF extract(month from NEW.time_start)>=7 THEN
            NEW.semester=2;
        END IF;
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION time.time_calc_semester()
  OWNER TO "%db_admin%";

CREATE OR REPLACE FUNCTION time.time_calc_time_interval()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.time_period=age(NEW.time_end,NEW.time_start);
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION time.time_calc_time_interval()
  OWNER TO "%db_admin%";

  CREATE OR REPLACE FUNCTION time.time_calc_year()
  RETURNS trigger AS
$BODY$
 BEGIN
 NEW.year=extract(year from NEW.time_start);
 RETURN NEW;
 END;
 $BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;
ALTER FUNCTION time.time_calc_year()
  OWNER TO "%db_admin%";


CREATE TRIGGER trigg_time_calc_decade
  BEFORE INSERT OR UPDATE
  ON "time"."time"
  FOR EACH ROW
  EXECUTE PROCEDURE time.time_calc_decade();


CREATE TRIGGER trigg_time_calc_month
  BEFORE INSERT OR UPDATE
  ON "time"."time"
  FOR EACH ROW
  EXECUTE PROCEDURE time.time_calc_month();


CREATE TRIGGER trigg_time_calc_month_name
  BEFORE INSERT OR UPDATE
  ON "time"."time"
  FOR EACH ROW
  EXECUTE PROCEDURE time.time_calc_month_name();


CREATE TRIGGER trigg_time_calc_quarter
  BEFORE INSERT OR UPDATE
  ON "time"."time"
  FOR EACH ROW
  EXECUTE PROCEDURE time.time_calc_quarter();


CREATE TRIGGER trigg_time_calc_semester
  BEFORE INSERT OR UPDATE
  ON "time"."time"
  FOR EACH ROW
  EXECUTE PROCEDURE time.time_calc_semester();

CREATE TRIGGER trigg_time_calc_time_interval
  BEFORE INSERT OR UPDATE
  ON "time"."time"
  FOR EACH ROW
  EXECUTE PROCEDURE time.time_calc_time_interval();


CREATE TRIGGER trigg_time_calc_year
  BEFORE INSERT OR UPDATE
  ON "time"."time"
  FOR EACH ROW
  EXECUTE PROCEDURE time.time_calc_year();


INSERT INTO time.time(time_start,time_end) VALUES ('1918-01-01','1919-01-01');

