-- Authors: Richard D Boyce, Erica Voss, Lee Evans
-- 2014/2015
-- Postgresql script

DROP TABLE IF EXISTS DRUG_HOI_EVIDENCE;
DROP SEQUENCE IF EXISTS DRUG_HOI_EVIDENCE_SEQUENCE;
CREATE SEQUENCE DRUG_HOI_EVIDENCE_SEQUENCE MAXVALUE 9223372036854775807 NO CYCLE;
CREATE TABLE DRUG_HOI_EVIDENCE (
    ID                          INTEGER NOT NULL DEFAULT NEXTVAL('DRUG_HOI_EVIDENCE_SEQUENCE'),
    DRUG_HOI_RELATIONSHIP       VARCHAR(50),
    EVIDENCE_TYPE               VARCHAR(4000),
    MODALITY                    VARCHAR(1),
    EVIDENCE_SOURCE_CODE_ID     INTEGER,
    STATISTIC_VALUE             NUMERIC NOT NULL,
    EVIDENCE_LINKOUT            VARCHAR(4000),
    STATISTIC_TYPE              VARCHAR(4000) 
);
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.ID IS 'primary key';
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.DRUG_HOI_RELATIONSHIP IS 'foreign key to the drug_HOI_relationship id';
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.EVIDENCE_TYPE IS 'the type of evidence (literature, product label, pharmacovigilance, EHR)';
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.MODALITY IS 'Whether or not the relationship of evidence is to refute the assertion';
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.EVIDENCE_SOURCE_CODE_ID IS 'a code indicating the actual source of evidence (e.g., PubMed, US SPLs, EU SPC, VigiBase, etc)';
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.STATISTIC_VALUE IS 'For literature-like (e.g., PubMed abstracts, product labeling) sources this holds the count of the number of items of the evidence type present in the evidence base from that source (several rules are used to derive the counts, see documentation on the knowledge-base wiki). From signal detection sources, the result of applying the algorithm indicated in the evidence_type column is shown.';
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.EVIDENCE_LINKOUT IS 'For literature-like (e.g., PubMed abstracts, product labeling), this holds a URL that will resolve to a query against the RDF endpoint for all resources used to generate the evidence_count. For signal detection sources, this holds a link to metadata on the algorithm and how it was applied to arrive at the statistical value.';
COMMENT ON COLUMN DRUG_HOI_EVIDENCE.STATISTIC_TYPE IS 'For literature-like (e.g., PubMed abstracts, product labeling), and other count based methods this holds COUNT. For signal detection sources, this holds a string indicating the type of the result value (e.g., AERS_EBGM, AERS_EB05)';

DROP TABLE IF EXISTS EVIDENCE_SOURCES;
DROP SEQUENCE IF EXISTS EVIDENCE_SOURCES_SEQUENCE;
CREATE SEQUENCE EVIDENCE_SOURCES_SEQUENCE MAXVALUE 9223372036854775807 NO CYCLE;
CREATE TABLE EVIDENCE_SOURCES (
    ID                          INTEGER NOT NULL DEFAULT NEXTVAL('EVIDENCE_SOURCES_SEQUENCE'),
    TITLE                       VARCHAR(4000),
    DESCRIPTION                 VARCHAR(4000),
    CONTRIBUTER                 VARCHAR(4000),
    CREATOR                     VARCHAR(4000),
    CREATION_DATE               DATE NOT NULL, 
    RIGHTS                      VARCHAR(4000),
    SOURCE                      VARCHAR(4000)
);
COMMENT ON COLUMN EVIDENCE_SOURCES.TITLE IS 'a short name for the evidence source. Same as http://purl.org/dc/elements/1.1/title';
COMMENT ON COLUMN EVIDENCE_SOURCES.DESCRIPTION IS 'Description of the evidence source. Same as http://purl.org/dc/elements/1.1/description';
COMMENT ON COLUMN EVIDENCE_SOURCES.CONTRIBUTER IS 'Same as http://purl.org/dc/elements/1.1/contributor';
COMMENT ON COLUMN EVIDENCE_SOURCES.CREATOR IS 'Same as http://purl.org/dc/elements/1.1/creator';
COMMENT ON COLUMN EVIDENCE_SOURCES.CREATION_DATE IS 'Date that the source was created. For example, if the source was created in 2010 but added to the knowledge base in 2014, the creation date would be 2010';
COMMENT ON COLUMN EVIDENCE_SOURCES.RIGHTS IS 'Same as http://purl.org/dc/elements/1.1/rights';
COMMENT ON COLUMN EVIDENCE_SOURCES.SOURCE IS 'The source from which this data was derived. Same as http://purl.org/dc/elements/1.1/source';

DROP TABLE IF EXISTS DRUG_HOI_RELATIONSHIP;
CREATE TABLE DRUG_HOI_RELATIONSHIP (
    ID                          VARCHAR(50),
    DRUG                        INTEGER,
    RXNORM_DRUG                 VARCHAR(4000),
    HOI                         INTEGER,
    SNOMED_HOI                  VARCHAR(4000)
);
COMMENT ON COLUMN DRUG_HOI_RELATIONSHIP.DRUG IS 'OMOP/IMEDS Concept ID for the drug';
COMMENT ON COLUMN DRUG_HOI_RELATIONSHIP.RXNORM_DRUG IS 'RxNorm Preferred Term of the Drug';
COMMENT ON COLUMN DRUG_HOI_RELATIONSHIP.HOI IS 'OMOP/IMEDS Concept ID for the Health Outcome of Interest';
COMMENT ON COLUMN DRUG_HOI_RELATIONSHIP.SNOMED_HOI IS 'SNOMED preferred term of the Health Outcome of Interest';

DROP TABLE IF EXISTS LAERTES_SUMMARY;
DROP SEQUENCE IF EXISTS LAERTES_SUMMARY_SEQUENCE;
CREATE SEQUENCE LAERTES_SUMMARY_SEQUENCE MAXVALUE 9223372036854775807 NO CYCLE;
CREATE TABLE LAERTES_SUMMARY (
	ID                      INTEGER NOT NULL DEFAULT NEXTVAL('LAERTES_SUMMARY_SEQUENCE'),
	REPORT_ORDER            INTEGER,
	REPORT_NAME             VARCHAR(4000),
	INGREDIENT_ID           INTEGER,
	INGREDIENT              VARCHAR(4000),
	CLINICAL_DRUG_ID        INTEGER,
	CLINICAL_DRUG           VARCHAR(4000),
	HOI_ID                  INTEGER,
	HOI                     VARCHAR(4000),
	CT_COUNT                INTEGER,
	CASE_COUNT              INTEGER,
	OTHER_COUNT             INTEGER,
	SPLICER_COUNT           INTEGER,
	EU_SPC_COUNT            INTEGER,
	SEMMEDDB_CT_COUNT       INTEGER,
	SEMMEDDB_CASE_COUNT	INTEGER,
	SEMMEDDB_NEG_CT_COUNT	INTEGER,
	SEMMEDDB_NEG_CASE_COUNT	INTEGER,
	EB05			NUMERIC,
	EBGM			NUMERIC,
	AERS_REPORT_COUNT	INTEGER
);
COMMENT ON COLUMN LAERTES_SUMMARY.ID IS 'primary key';
COMMENT ON COLUMN LAERTES_SUMMARY.REPORT_ORDER IS 'there are several reports in this summary, this is an identifier for each report';
COMMENT ON COLUMN LAERTES_SUMMARY.REPORT_NAME IS 'there are several reports in this summary, this is a name of the report';
COMMENT ON COLUMN LAERTES_SUMMARY.INGREDIENT_ID IS 'a drug ingredient CONCEPT_ID';
COMMENT ON COLUMN LAERTES_SUMMARY.INGREDIENT IS 'a drug ingredient name';
COMMENT ON COLUMN LAERTES_SUMMARY.CLINICAL_DRUG_ID IS 'if a clinical drug exists, the clinical drug CONCEPT_ID';
COMMENT ON COLUMN LAERTES_SUMMARY.CLINICAL_DRUG IS 'if a clinical drug exists, the clinical drug name';
COMMENT ON COLUMN LAERTES_SUMMARY.HOI_ID IS 'the HOI CONCEPT_ID, this is at the SNOMED level';
COMMENT ON COLUMN LAERTES_SUMMARY.HOI IS 'the HOI name, this is at the SNOMED level';
COMMENT ON COLUMN LAERTES_SUMMARY.CT_COUNT IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.CASE_COUNT IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.OTHER_COUNT IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.SPLICER_COUNT IS 'counts of SPLs that mention specific drugs and hois';
COMMENT ON COLUMN LAERTES_SUMMARY.EU_SPC_COUNT IS 'counts of SPCs that mention specific drugs and hois';
COMMENT ON COLUMN LAERTES_SUMMARY.SEMMEDDB_CT_COUNT IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.SEMMEDDB_CASE_COUNT IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.SEMMEDDB_NEG_CT_COUNT IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.SEMMEDDB_NEG_CASE_COUNT IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.EB05 IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.EBGM IS '';
COMMENT ON COLUMN LAERTES_SUMMARY.AERS_REPORT_COUNT IS '';

-- add EVIDENCE_SOURCES table constraints
ALTER TABLE EVIDENCE_SOURCES ADD CONSTRAINT PK_EVIDENCE_SOURCES PRIMARY KEY (ID);

-- add DRUG_HOI_RELATIONSHIP table constraints
ALTER TABLE DRUG_HOI_RELATIONSHIP ADD CONSTRAINT PK_DRUG_HOI_RELATIONSHIP PRIMARY KEY (ID);

-- add LAERTES_SUMMARY table constraints
ALTER TABLE LAERTES_SUMMARY ADD CONSTRAINT PK_LAERTES_SUMMARY PRIMARY KEY (ID);

-- add DRUG_HOI_EVIDENCE table constraints
ALTER TABLE DRUG_HOI_EVIDENCE ADD CONSTRAINT FK_DRUG_HOI_RELATIONSHIP FOREIGN KEY (DRUG_HOI_RELATIONSHIP) REFERENCES DRUG_HOI_RELATIONSHIP(ID);
ALTER TABLE DRUG_HOI_EVIDENCE ADD CONSTRAINT FK_EVIDENCE_SOURCES FOREIGN KEY (EVIDENCE_SOURCE_CODE_ID) REFERENCES EVIDENCE_SOURCES(ID);
ALTER TABLE DRUG_HOI_EVIDENCE ADD CONSTRAINT PK_DRUG_HOI_EVIDENCE PRIMARY KEY (ID);
