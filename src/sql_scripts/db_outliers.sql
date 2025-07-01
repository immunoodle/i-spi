CREATE SCHEMA IF NOT EXISTS madi_lumi_reader_outliers;

CREATE TABLE madi_lumi_reader_outliers.main_context (
    id SERIAL PRIMARY KEY,
    workspace_id INTEGER NOT NULL,  -- Assuming workspace_id remains an integer
    study TEXT NOT NULL,            -- Changed from INTEGER to TEXT
    experiment TEXT NOT NULL,       -- Changed from INTEGER to TEXT
    context_name TEXT               -- Optional: Name or description of the context if needed
);

CREATE TABLE madi_lumi_reader_outliers.comparisons (
    id SERIAL PRIMARY KEY,
    context_id INTEGER NOT NULL REFERENCES madi_lumi_reader_outliers.main_context(id),
    antigen TEXT NOT NULL,
    visit_1 TEXT NOT NULL,
    visit_2 TEXT NOT NULL,
    serialized_plot BYTEA NOT NULL, -- Use BYTEA for binary data like serialized R objects

    -- Unique constraint to avoid duplicate antigen-visit combinations in a single context
    CONSTRAINT unique_antigen_visits UNIQUE(context_id, antigen, visit_1, visit_2)
);

CREATE TABLE madi_lumi_reader_outliers.outliers (
    id SERIAL PRIMARY KEY,
    comparison_id INTEGER NOT NULL REFERENCES madi_lumi_reader_outliers.comparisons(id),
    subject_accession TEXT NOT NULL,  -- Assuming this is required for each outlier
    visit_1 TEXT NOT NULL,            -- To reflect "Visit : {visit_1}"
    visit_2 TEXT NOT NULL,            -- To reflect "Visit : {visit_2}"
    gate_class TEXT,                  -- Assuming this may be nullable
    hample_outlier BOOLEAN,           -- Assuming outliers can be represented as boolean
    bagplot_outlier BOOLEAN,          -- Same as above
    kde_outlier BOOLEAN,              -- Same as above
    antigen TEXT NOT NULL,            -- Name of the antigen
    feature TEXT NOT NULL,            -- Name of the feature
    additional_info JSONB             -- Optional, for any extra data
);


-- Index for workspace_id in the main_context table to speed up queries filtering by workspace
CREATE INDEX idx_main_context_workspace_id ON madi_lumi_reader_outliers.main_context(workspace_id);

-- Indexes for studies and experiments if these fields will be queried frequently
CREATE INDEX idx_main_context_study ON madi_lumi_reader_outliers.main_context(study);
CREATE INDEX idx_main_context_experiment ON madi_lumi_reader_outliers.main_context(experiment);

-- Index for context_id in the comparisons table to speed up joins and filter operations
CREATE INDEX idx_comparisons_context_id ON madi_lumi_reader_outliers.comparisons(context_id);

-- Indexes for visits and antigen in the comparisons table to speed up querying by these fields
CREATE INDEX idx_comparisons_visit_1 ON madi_lumi_reader_outliers.comparisons(visit_1);
CREATE INDEX idx_comparisons_visit_2 ON madi_lumi_reader_outliers.comparisons(visit_2);
CREATE INDEX idx_comparisons_antigen ON madi_lumi_reader_outliers.comparisons(antigen);

-- Index for comparison_id in the outliers table to optimize joins and lookups
CREATE INDEX idx_outliers_comparison_id ON madi_lumi_reader_outliers.outliers(comparison_id);

-- Indexes for visit_1, visit_2, and antigen in the outliers table to optimize filtering
CREATE INDEX idx_outliers_visit_1 ON madi_lumi_reader_outliers.outliers(visit_1);
CREATE INDEX idx_outliers_visit_2 ON madi_lumi_reader_outliers.outliers(visit_2);
CREATE INDEX idx_outliers_antigen ON madi_lumi_reader_outliers.outliers(antigen);

-- Index for subject_accession in the outliers table to speed up searches by this field
CREATE INDEX idx_outliers_subject_accession ON madi_lumi_reader_outliers.outliers(subject_accession);

-- Assuming "feature" is a significant field for queries or grouping operations
CREATE INDEX idx_outliers_feature ON madi_lumi_reader_outliers.outliers(feature);



-- Add value_type column to main_context table (without CHECK constraint)
ALTER TABLE madi_lumi_reader_outliers.main_context
ADD COLUMN value_type TEXT NOT NULL DEFAULT 'MFI';

-- Add unique constraint for the combination
ALTER TABLE madi_lumi_reader_outliers.main_context
ADD CONSTRAINT unique_context_combination
UNIQUE (workspace_id, study, experiment, value_type);

-- Add index for value_type
CREATE INDEX idx_main_context_value_type
ON madi_lumi_reader_outliers.main_context(value_type);


-- Add value_type column to comparisons table
ALTER TABLE madi_lumi_reader_outliers.comparisons
ADD COLUMN value_type TEXT NOT NULL DEFAULT 'MFI';

-- Add index for value_type
CREATE INDEX idx_comparisons_value_type
ON madi_lumi_reader_outliers.comparisons(value_type);

-- Add lab_confirmed column to outliers table
ALTER TABLE madi_lumi_reader_outliers.outliers
ADD COLUMN lab_confirmed BOOLEAN DEFAULT FALSE;

-- Optionally, create an index if you plan to query by this field frequently
CREATE INDEX idx_outliers_lab_confirmed
ON madi_lumi_reader_outliers.outliers(lab_confirmed);


-- First, rename the existing gate_class column to gate_class_1
ALTER TABLE madi_lumi_reader_outliers.outliers
RENAME COLUMN gate_class TO gate_class_1;

-- Then add gate_class_2 column
ALTER TABLE madi_lumi_reader_outliers.outliers
ADD COLUMN gate_class_2 TEXT;

-- Add index for the new gate_class columns
CREATE INDEX idx_outliers_gate_class_1
ON madi_lumi_reader_outliers.outliers(gate_class_1);

CREATE INDEX idx_outliers_gate_class_2
ON madi_lumi_reader_outliers.outliers(gate_class_2);


-- Add job_status column to main_context table
ALTER TABLE madi_lumi_reader_outliers.main_context
ADD COLUMN job_status TEXT NOT NULL DEFAULT 'pending';

-- Create an index for job_status for faster queries
CREATE INDEX idx_main_context_job_status
ON madi_lumi_reader_outliers.main_context(job_status);