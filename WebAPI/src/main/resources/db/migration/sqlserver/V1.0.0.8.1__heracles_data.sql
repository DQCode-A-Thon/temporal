ALTER TABLE HERACLES_VISUALIZATION_DATA DROP CONSTRAINT PK_heracles_viz_data
ALTER TABLE HERACLES_VISUALIZATION_DATA DROP COLUMN id;
ALTER TABLE HERACLES_VISUALIZATION_DATA ADD id integer IDENTITY(1,1);
ALTER TABLE HERACLES_VISUALIZATION_DATA ADD CONSTRAINT PK_heracles_viz_data
PRIMARY KEY CLUSTERED
(
    id ASC
)