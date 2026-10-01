-- Read-only user for the Shiny app (service "shiny" of the compose file).
-- Runs once, when the PostGIS volume is created (docker-entrypoint-initdb.d),
-- connected to the "gta" database as its owner ("gta").
-- For an existing volume, run the same statements by hand (see compose/README.md).
--
-- The password must match SHINY_DB_PASSWORD in the compose file (default: gta_reader).
-- Change both for any deployment reachable by others.

CREATE ROLE gta_reader LOGIN PASSWORD 'gta_reader';
GRANT CONNECT ON DATABASE gta TO gta_reader;

-- The schemas and tables do not exist yet: the workflow creates them later.
-- Default privileges give gta_reader read access to everything "gta" creates.
ALTER DEFAULT PRIVILEGES FOR ROLE gta GRANT USAGE ON SCHEMAS TO gta_reader;
ALTER DEFAULT PRIVILEGES FOR ROLE gta GRANT SELECT ON TABLES TO gta_reader;

-- Schemas that already exist (public, created by PostGIS).
GRANT USAGE ON SCHEMA public TO gta_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO gta_reader;
