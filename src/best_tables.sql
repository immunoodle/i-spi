--- best_plate_all
CREATE TABLE IF NOT EXISTS madi_results.best_plate_all
(
    best_plate_all_id bigint NOT NULL DEFAULT nextval('madi_results.best_plate_all_id_seq'::regclass),
    study_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    experiment_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    feature character varying(15) COLLATE pg_catalog."default",
    source character varying(25) COLLATE pg_catalog."default",
    plateid character varying(2024) COLLATE pg_catalog."default",
    plate character varying(15) COLLATE pg_catalog."default",
    sample_dilution_factor numeric,
    assay_response_variable character varying(48) COLLATE pg_catalog."default",
    assay_independent_variable character varying(48) COLLATE pg_catalog."default",
    nominal_sample_dilution character varying(128) COLLATE pg_catalog."default",
    project_id integer,
    wavelength character varying(15) COLLATE pg_catalog."default" DEFAULT '__none__'::character varying,
    CONSTRAINT best_plate_all_pkey PRIMARY KEY (best_plate_all_id),
    CONSTRAINT best_plate_all_nk UNIQUE (project_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, source, wavelength)
)

TABLESPACE pg_default;


--- best_glance_all
CREATE TABLE IF NOT EXISTS madi_results.best_glance_all
(
    best_glance_all_id bigint NOT NULL DEFAULT nextval('madi_results.best_glance_all_id_seq'::regclass),
    study_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    experiment_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    plateid character varying(100) COLLATE pg_catalog."default" NOT NULL,
    plate character varying(15) COLLATE pg_catalog."default",
    sample_dilution_factor numeric,
    antigen character varying(64) COLLATE pg_catalog."default",
    iter numeric,
    status character varying(100) COLLATE pg_catalog."default",
    crit character varying(20) COLLATE pg_catalog."default",
    a numeric,
    b numeric,
    c numeric,
    d numeric,
    lloq numeric,
    uloq numeric,
    lloq_y numeric,
    uloq_y numeric,
    llod numeric,
    ulod numeric,
    inflect_x numeric,
    inflect_y numeric,
    std_error_blank numeric,
    dydx_inflect numeric,
    dfresidual numeric,
    nobs numeric,
    rsquare_fit numeric,
    aic numeric,
    bic numeric,
    loglik numeric,
    mse double precision,
    cv double precision,
    source character varying(25) COLLATE pg_catalog."default",
    bkg_method character varying COLLATE pg_catalog."default",
    is_log_response boolean,
    is_log_x boolean,
    apply_prozone boolean,
    formula text COLLATE pg_catalog."default",
    g numeric,
    nominal_sample_dilution character varying(128) COLLATE pg_catalog."default",
    project_id integer,
    mindc numeric,
    minrdl numeric,
    maxdc numeric,
    maxrdl numeric,
    feature character varying(15) COLLATE pg_catalog."default",
    last_concentration_calc_method character varying(50) COLLATE pg_catalog."default",
    wavelength character varying(15) COLLATE pg_catalog."default" DEFAULT '__none__'::character varying,
    lloq_fda2018_response double precision,
    lloq_fda2018_concentration double precision,
    uloq_fda2018_response double precision,
    uloq_fda2018_concentration double precision,
    blank_mean double precision,
    blank_sd double precision,
    CONSTRAINT best_glance_all_pkey PRIMARY KEY (best_glance_all_id),
    CONSTRAINT best_glance_all_nk UNIQUE (project_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, source, wavelength, antigen, feature)
)

TABLESPACE pg_default;

--- best_tidy_all
CREATE TABLE IF NOT EXISTS madi_results.best_tidy_all
(
    best_tidy_all_id bigint NOT NULL DEFAULT nextval('madi_results.best_tidy_all_id_seq'::regclass),
    study_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    experiment_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    term character varying(15) COLLATE pg_catalog."default",
    lower numeric,
    upper numeric,
    estimate numeric,
    std_error numeric,
    statistic numeric,
    p_value numeric,
    sample_dilution_factor numeric,
    antigen character varying(64) COLLATE pg_catalog."default",
    plateid character varying(100) COLLATE pg_catalog."default" NOT NULL,
    plate character varying(15) COLLATE pg_catalog."default",
    source character varying(25) COLLATE pg_catalog."default",
    nominal_sample_dilution character varying(128) COLLATE pg_catalog."default",
    project_id integer,
    feature character varying(15) COLLATE pg_catalog."default",
    wavelength character varying(15) COLLATE pg_catalog."default" DEFAULT '__none__'::character varying,
    CONSTRAINT best_tidy_all_pkey PRIMARY KEY (best_tidy_all_id),
    CONSTRAINT best_tidy_all_nk UNIQUE (project_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, source, wavelength, antigen, feature, term)
)

TABLESPACE pg_default;

--- best_standard_all
CREATE TABLE IF NOT EXISTS madi_results.best_standard_all
(
    best_standard_all_id bigint NOT NULL DEFAULT nextval('madi_results.best_standard_all_id_seq'::regclass),
    study_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    experiment_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    feature character varying(15) COLLATE pg_catalog."default",
    source character varying(25) COLLATE pg_catalog."default",
    plateid character varying(100) COLLATE pg_catalog."default",
    plate character varying(40) COLLATE pg_catalog."default",
    stype character varying(6) COLLATE pg_catalog."default" NOT NULL,
    sample_dilution_factor numeric,
    sampleid character varying(15) COLLATE pg_catalog."default" NOT NULL,
    well character varying(50) COLLATE pg_catalog."default" NOT NULL,
    dilution numeric(9,0),
    antigen character varying(64) COLLATE pg_catalog."default" NOT NULL,
    assay_response numeric,
    assay_response_variable character varying(48) COLLATE pg_catalog."default",
    assay_independent_variable character varying(48) COLLATE pg_catalog."default",
    concentration numeric,
    g numeric,
    best_glance_all_id bigint,
    nominal_sample_dilution character varying(128) COLLATE pg_catalog."default",
    project_id integer,
    wavelength character varying(15) COLLATE pg_catalog."default" DEFAULT '__none__'::character varying,
    CONSTRAINT best_standard_all_pkey PRIMARY KEY (best_standard_all_id),
    CONSTRAINT best_standard_all_nk UNIQUE (project_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, source, wavelength, antigen, feature, well)
)

TABLESPACE pg_default;

--- best_pred_all
CREATE TABLE IF NOT EXISTS madi_results.best_pred_all
(
    best_pred_all_id bigint NOT NULL DEFAULT nextval('madi_results.best_pred_all_id_seq'::regclass),
    x numeric,
    model character varying(20) COLLATE pg_catalog."default",
    yhat numeric,
    overall_se numeric,
    predicted_concentration numeric,
    se_x numeric,
    pcov numeric,
    study_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    experiment_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    plateid character varying(100) COLLATE pg_catalog."default",
    plate character varying(40) COLLATE pg_catalog."default",
    antigen character varying(64) COLLATE pg_catalog."default",
    source character varying(25) COLLATE pg_catalog."default",
    best_glance_all_id bigint,
    nominal_sample_dilution character varying(128) COLLATE pg_catalog."default",
    project_id integer,
    raw_robust_concentration numeric,
    final_robust_concentration numeric,
    se_robust_concentration numeric,
    pcov_robust_concentration numeric,
    feature character varying(15) COLLATE pg_catalog."default",
    wavelength character varying(15) COLLATE pg_catalog."default" DEFAULT '__none__'::character varying,
    CONSTRAINT best_pred_all_pkey PRIMARY KEY (best_pred_all_id),
    CONSTRAINT best_pred_all_nk UNIQUE (project_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, source, wavelength, antigen, feature, x)
)

TABLESPACE pg_default;

---best_sample_se_all
CREATE TABLE IF NOT EXISTS madi_results.best_sample_se_all
(
    best_sample_se_all_id bigint NOT NULL DEFAULT nextval('madi_results.best_sample_se_all_id_seq'::regclass),
    raw_predicted_concentration numeric,
    study_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    experiment_accession character varying(15) COLLATE pg_catalog."default" NOT NULL,
    timeperiod character varying(40) COLLATE pg_catalog."default",
    patientid character varying(15) COLLATE pg_catalog."default",
    well character varying(6) COLLATE pg_catalog."default" NOT NULL,
    stype character varying(6) COLLATE pg_catalog."default" NOT NULL,
    sampleid character varying(15) COLLATE pg_catalog."default" NOT NULL,
    agroup character varying(40) COLLATE pg_catalog."default",
    pctaggbeads numeric(8,0),
    samplingerrors character varying(64) COLLATE pg_catalog."default",
    antigen character varying(64) COLLATE pg_catalog."default" NOT NULL,
    antibody_n integer,
    plateid character varying(100) COLLATE pg_catalog."default",
    plate character varying(40) COLLATE pg_catalog."default",
    sample_dilution_factor numeric,
    assay_response_variable character varying(48) COLLATE pg_catalog."default",
    assay_independent_variable character varying(48) COLLATE pg_catalog."default",
    dilution numeric(9,0) NOT NULL,
    overall_se numeric,
    assay_response numeric,
    se_concentration numeric,
    final_predicted_concentration double precision,
    pcov numeric,
    source character varying(25) COLLATE pg_catalog."default",
    gate_class_loq character varying(50) COLLATE pg_catalog."default",
    gate_class_lod character varying(50) COLLATE pg_catalog."default",
    gate_class_pcov character varying(50) COLLATE pg_catalog."default",
    norm_assay_response numeric,
    best_glance_all_id bigint,
    feature character varying(15) COLLATE pg_catalog."default",
    plate_id character varying(640) COLLATE pg_catalog."default",
    nominal_sample_dilution character varying(128) COLLATE pg_catalog."default",
    project_id integer,
    raw_assay_response numeric,
    raw_robust_concentration numeric,
    final_robust_concentration numeric,
    se_robust_concentration numeric,
    pcov_robust_concentration numeric,
    wavelength character varying(15) COLLATE pg_catalog."default" DEFAULT '__none__'::character varying,
    CONSTRAINT best_sample_se_all_pkey PRIMARY KEY (best_sample_se_all_id),
    CONSTRAINT best_sample_se_all_nk UNIQUE (project_id, study_accession, experiment_accession, plateid, plate, nominal_sample_dilution, source, wavelength, antigen, feature, patientid, timeperiod, sampleid, dilution)
)

TABLESPACE pg_default;
