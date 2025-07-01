SELECT DISTINCT experiment_accession, plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_buffer
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
	
UPDATE madi_results.xmap_buffer
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_buffer
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_buffer.plate_id = a.plate_id;

SELECT DISTINCT experiment_accession, plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_control
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_control
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_control
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_control.plate_id = a.plate_id;

SELECT DISTINCT experiment_accession, plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_header
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_header
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_header
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_header.plate_id = a.plate_id;

SELECT DISTINCT experiment_accession, plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_sample
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_sample
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_sample
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_sample.plate_id = a.plate_id;

SELECT DISTINCT experiment_accession, plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_standard
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_standard
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plate_id, CONCAT(experiment_accession, '_', split_part(CASE WHEN length(split_part(plate_id, '\',11))>5 THEN split_part(plate_id, '\',11) ELSE split_part(plate_id, '\',10) END, ' ', 4)) AS new_experiment
	FROM madi_results.xmap_standard
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_standard.plate_id = a.plate_id;

SELECT DISTINCT experiment_accession, plateid, split_part(plateid,'.',4)
	FROM madi_results.xmap_standard_fits
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
	
SELECT DISTINCT experiment_accession, plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_fits
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_standard_fits
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_fits
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_standard_fits.plateid = a.plateid;

SELECT DISTINCT experiment_accession, plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_fit_tab
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_standard_fit_tab
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_fit_tab
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_standard_fit_tab.plateid = a.plateid;

SELECT DISTINCT experiment_accession, plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_preds
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_standard_preds
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_preds
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_standard_preds.plateid = a.plateid;

SELECT DISTINCT experiment_accession, plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_stor
	WHERE study_accession = 'MADI_01' AND split_part(experiment_accession, '_', 1) = 'ADCD';
UPDATE madi_results.xmap_standard_stor
SET experiment_accession = a.new_experiment
FROM (SELECT DISTINCT plateid, CONCAT(experiment_accession, '_', split_part(plateid,'.',4)) AS new_experiment
	FROM madi_results.xmap_standard_stor
	WHERE study_accession = 'MADI_01' AND experiment_accession = 'ADCD') AS a
WHERE xmap_standard_stor.plateid = a.plateid;

