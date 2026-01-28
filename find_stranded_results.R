$ Rscript find_stranded.R .
[find_stranded] Scanning app at C:\Users\d78039e\Documents\R-git\i-spi-loader2026f
[static] Found 49 R source files
[static] Discovered 606 function definitions
[static] Unique function names: 487
[static] Found calls to 279 of our defined functions
[static] Identified 14 likely Shiny entry points

=== STATIC CALL-GRAPH RESULT ==============================
Total defined functions : 487
Functions with calls    : 279
Entry point functions   : 14
Live functions          : 300
Potentially dead        : 187

=== POTENTIALLY STRANDED FUNCTIONS ========================

File: find_stranded.R
  Line   35: collect_function_definitions()
  Line   37: error()
  Line   87: error()
  Line  141: find_reactive_entries()
  Line  143: error()
  Line  171: is_likely_entry_point()

File: app.R
  Line  787: shinyValue()

File: batch_fit_functions.R
  Line    1: fetch_db_header_experiments()
  Line   12: build_antigen_list()
  Line   53: build_antigen_plate_list()
  Line   97: prep_plate_data_batch()
  Line  137: fit_experiment_plate_batch()
  Line  338: process_batch_outputs()

File: batch_layout_functions.R
  Line    1: reset_batch_reactives()
  Line  260: onSessionStart()
  Line  425: add_clean_plate_id_from_filename()
  Line  432: add_clean_plate_id_from_plate_filename()
  Line  445: test_clean_plate_id()
  Line  656: apply_default_values_to_plates_map()
  Line  823: validate_batch_description()
  Line  879: process_uploaded_files()
  Line  919: process_uploaded_files_v2()
  Line  985: add_source_column()
  Line 1125: generate_layout_template()
  Line 2236: check_sheet_names()
  Line 2501: import_layout_file()
  Line 2596: transpose_batch_header()
  Line 2624: construct_batch_upload_metadata()
  Line 2946: combine_plate_metadata()
  Line 2961: prepare_batch_bead_assay_samples()
  Line 3047: prepare_batch_bead_assay_standards()
  Line 3105: prepare_batch_bead_assay_blanks()
  Line 3156: prepare_batch_bead_assay_controls()
  Line 3211: prepare_batch_antigen_family()
  Line 3222: prepare_planned_visits()
  Line 3236: prepare_batch_header()

File: bead_count_analysis_ui.R
  Line  315: filename()
  Line  318: content()

File: bead_count_functions.R
  Line  255: download_bead_count_data()

File: db_functions.R
  Line  145: shiny_notify()
  Line  296: upsert_best_curve()
  Line  495: pg_type_map()
  Line  548: build_upsert_sql_glue()
  Line  670: fetch_best_tidy_all()
  Line  678: fetch_best_pred_all()
  Line  688: fetch_best_standard_all()
  Line  698: fetch_best_glance_all()
  Line  748: fetch_best_sample_se_all()
  Line  873: fetch_current_sc_options_wide()

File: dilution_analysis_ui.R
  Line  492: filename()
  Line  495: content()
  Line  941: filename()
  Line  944: content()
  Line  967: filename()
  Line  970: content()

File: dilution_linearity_functions.R
  Line    2: get_dilution_parameters()
  Line   10: fetch_sample_data_linearity()
  Line  182: calculate_sample_concentration_status()
  Line  293: join_sample_standard_data()
  Line  820: classify_sample_data()
  Line  861: plot_patient_dilution_series()
  Line 1096: download_processed_dilution_data()
  Line 1475: parse_leaf_path()
  Line 1516: classify_sample()
  Line 1545: download_classified_sample()
  Line 1560: download_dilution_contigency_summary_fun()
  Line 1576: plot_classification_heatmap()
  Line 1627: obtain_passing_subject_contigency()
  Line 1653: obtain_contigency_table()
  Line 1838: obtain_passing_patients()
  Line 1849: obtain_passing_dilutions_df()
  Line 1890: compute_average_au()
  Line 1906: preserve_all_au()
  Line 1918: preserve_passing_au()
  Line 1925: geometric_mean_all_au_2()
  Line 1961: geometric_mean_passing_au_2()
  Line 1980: geometric_mean_positive_controls()
  Line 2005: geometric_mean_blanks()
  Line 2024: save_average_au()
  Line 2086: is_optimization_plate()
  Line 2093: is_optimization_experiment_parsed()
  Line 2211: split_optimization_single_upload()
  Line 2439: split_optimization_plates()
  Line 2859: fit_model()
  Line 2898: fit_model_xy()
  Line 2979: fit_raw_assay_response_model()
  Line 3304: plot_single_regres()
  Line 3574: dilution_lm_facet()
  Line 3616: produce_all_plate_facets()

File: dilutional_linearity_ui.R
  Line  498: filename()
  Line  501: content()
  Line  517: filename()
  Line  520: content()
  Line  540: filename()
  Line  543: content()

File: global.R
  Line  212: reloadReactive()

File: helpers.R
  Line    2: createLink()
  Line  422: map_data_types()
  Line  575: generate_header()
  Line  602: process_samples_with_class()
  Line  648: calculate_x_min_max_dynamic()
  Line  668: welsch.weight()
  Line  835: compute_allplate_robust_curves()
  Line  984: createDownloadPlotData()
  Line  986: filename()
  Line  989: content()

File: import_lumifile.R
  Line  875: filename()
  Line  878: content()

File: load_previous_stored_data.R
  Line    4: createDownloadHandler()
  Line    6: filename()
  Line    9: content()
  Line  685: check_nsample_plate()
  Line  741: validate_plate_data()

File: model_functions.R
  Line   13: d2xY5()
  Line   32: d2xY4()
  Line   48: d2xYd5()
  Line   66: d2xYd4()
  Line   80: d2xYgomp4()
  Line   94: dxdyiY5()
  Line  111: dxdyiYd5()
  Line  148: dxdyiY4()
  Line  161: dxdyiYd4()
  Line  174: dxdyiYgomp4()

File: outlier_ui1.R
  Line    1: hidden()
  Line   23: checkExistingValueTypes()
  Line   51: preLoadData()
  Line  452: log_msg()
  Line  607: filename()
  Line  612: content()
  Line  848: filename()
  Line  851: content()
  Line 1157: onFulfilled()

File: outliers.R
  Line 1074: filename()
  Line 1077: content()

File: plate_validator_functions.R
  Line   29: check_type_column()
  Line   60: check_sample_description()
  Line   78: check_standard_description()
  Line   94: validate_batch_plate()
  Line  176: validate_batch_plate_metadata()
  Line  281: validate_batch_bead_array()
  Line  407: validate_batch_bead_array_data()
  Line  598: check_agg_bead_column()
  Line  706: validate_required_columns()
  Line  763: get_null_rows()
  Line  785: debug_sample_preparation()
  Line  826: check_existing_plates()
  Line  860: get_existing_antigens()
  Line  878: get_existing_visits()
  Line  900: insert_new_rows()
  Line  925: insert_to_table()
  Line  996: upload_specimen_data()
  Line 1115: upload_antigen_family()
  Line 1139: upload_planned_visits()
  Line 1164: upload_batch_to_database()
  Line 1325: plate_validation()
  Line 1443: createValidateBadge()
  Line 1464: createValidateBatchBadge()
  Line 1484: createUploadedBatchBadge()
  Line 1505: createOptimizedBadge()

File: plot_functions.R
  Line  147: plot_model_comparisons()
  Line  266: plot_standard_curve()
  Line  601: glance_plot_data()
  Line  626: make_feature_plot()

File: plot_norm_functions.R
  Line   35: produce_buffer_controls_data()
  Line   53: produce_controls_data()
  Line   60: load_norm_plate_data()

File: se_x_robust_fix.R
  Line  151: grad_inv_Y5_fixed_robust()
  Line  161: grad_inv_Yd5_fixed_robust()
  Line  170: grad_y_Y5_fixed_robust()
  Line  178: grad_y_Yd5_fixed_robust()
  Line  361: propagate_error_analytic_robust()

File: segment_reader.R
  Line  876: parse_plate_header()
  Line 1437: error()
  Line 1584: error()
  Line 1731: error()

File: std_curve_functions.R
  Line  373: compute_log_response()
  Line  449: obtain_response_variable()
  Line  608: error()
  Line  765: error()
  Line  974: interpolate_vertex()
  Line 1234: calculate_predicted_concentration()
  Line 2083: scale_pcov()
  Line 2340: test_se_at_inflection()

File: std_curver_summary_functions.R
  Line    1: aggregate_params()
  Line  379: summarize_sc_fits_plotly()
  Line  606: conduct_linear_interpolation()
  Line  621: compute_aggregated_curves()
  Line  649: conduct_linear_interpolation_batch()

File: std_curver_summary_ui.R
  Line  329: filename()
  Line  332: content()

File: std_curver_ui.R
  Line  578: filename()
  Line  582: content()
  Line  599: filename()
  Line  603: content()
  Line  659: filename()
  Line  663: content()

File: study_configuration.R
  Line   32: obtain_all_sc_source()

File: study_configuration_ui.R
  Line 1617: filename()
  Line 1620: content()

File: study_overview_functions.R
  Line   11: gn()
  Line  640: summarise_by_plate_id()
  Line  695: summarise_by_timeperiod()
  Line 1455: make_interplate_summ_spec()
  Line 1460: pivot_by_plate()
  Line 1466: pivot_sample_col()
  Line 1473: report_vars()
  Line 1557: get_bg_color()
  Line 1563: select_safe()
  Line 1896: make_timeperiod_grid()
  Line 1956: make_cv_scatterplot()
  Line 2012: plot_preped_analyte_fit_summary()
  Line 2439: make_antigen_plate_bead()

File: study_overview_ui.R
  Line  587: filename()
  Line  590: content()
  Line  601: filename()
  Line  636: plot_analyte_plate_specimen()
  Line  672: filename()
  Line  675: content()
  Line  686: filename()
  Line  699: plot_inter_intra_cv()
  Line  746: filename()
  Line  749: content()
  Line  760: filename()
  Line  763: content()
  Line  865: plot_bead_count_summary_plot()
  Line  989: filename()
  Line  992: content()
  Line 1016: filename()
  Line 1019: content()
  Line 1225: plot_analyte_plate_model()
  Line 1241: get_analyte_plate_proportion()
  Line 1257: filename()
  Line 1260: content()
  Line 1271: filename()
  Line 1274: content()
  Line 1282: filename()
  Line 1285: content()
  Line 1341: filename()
  Line 1344: content()
  Line 1380: filename()
  Line 1383: content()

File: subgroup_detection_summary_ui.R
  Line  429: filename()
  Line  433: content()

File: subgroup_detection_ui.R
  Line  527: filename()
  Line  530: content()
  Line  581: filename()
  Line  584: content()
  Line  628: filename()
  Line  631: content()
  Line  677: filename()
  Line  685: content()

File: subgroup_function.R
  Line   35: signed_euclidean()
  Line  991: download_first_visit_class()
  Line 1005: download_visit_difference()
  Line 1019: download_difference_histogram_data()
  Line 1031: download_assay_classification_data()

File: subgroup_summary_functions.R
  Line  132: grouped_antigens()
  Line  189: get_max_value()

File: ui_handler.R
  Line    2: reset_import_values()
  Line   24: get_user_projects()
  Line   35: get_user_projects_non_owner()
  Line  725: load_project()

File: xPonentReader.R
  Line  301: error()
  Line  388: error()
  Line  473: error()
  Line  556: error()

=== VERIFICATION ==========================================
Double-checking each 'dead' function for any references...

FALSE POSITIVE: fetch_db_header_experiments - found calls in:
   std_curver_ui.R:685
FALSE POSITIVE: build_antigen_list - found calls in:
   std_curver_ui.R:708
FALSE POSITIVE: build_antigen_plate_list - found calls in:
   std_curve_functions.R:1578
   std_curver_ui.R:711
FALSE POSITIVE: prep_plate_data_batch - found calls in:
   std_curver_ui.R:713
FALSE POSITIVE: fit_experiment_plate_batch - found calls in:
   std_curver_ui.R:719
FALSE POSITIVE: process_batch_outputs - found calls in:
   std_curver_ui.R:730
FALSE POSITIVE: reset_batch_reactives - found calls in:
   import_lumifile.R:1537
   import_lumifile.R:1547
   import_lumifile.R:1596
  ... and 2 more
FALSE POSITIVE: onSessionStart - found calls in:
   import_lumifile.R:1478
FALSE POSITIVE: test_clean_plate_id - found calls in:
   batch_layout_functions.R:443
FALSE POSITIVE: process_uploaded_files - found calls in:
   import_lumifile.R:893
   import_lumifile.R:2113
FALSE POSITIVE: add_source_column - found calls in:
   import_lumifile.R:2018
   import_lumifile.R:2102
FALSE POSITIVE: generate_layout_template - found calls in:
   import_lumifile.R:912
FALSE POSITIVE: check_sheet_names - found calls in:
   import_lumifile.R:2337
   import_lumifile.R:2605
   import_lumifile.R:2886
FALSE POSITIVE: import_layout_file - found calls in:
   import_lumifile.R:2348
   import_lumifile.R:2616
   import_lumifile.R:2897
FALSE POSITIVE: transpose_batch_header - found calls in:
   import_lumifile.R:2953
FALSE POSITIVE: construct_batch_upload_metadata - found calls in:
   import_lumifile.R:2449
   import_lumifile.R:2716
   import_lumifile.R:2946
FALSE POSITIVE: combine_plate_metadata - found calls in:
   import_lumifile.R:2965
FALSE POSITIVE: prepare_batch_bead_assay_samples - found calls in:
   plate_validator_functions.R:1030
FALSE POSITIVE: prepare_batch_bead_assay_standards - found calls in:
   plate_validator_functions.R:1054
FALSE POSITIVE: prepare_batch_bead_assay_blanks - found calls in:
   plate_validator_functions.R:1064
FALSE POSITIVE: prepare_batch_bead_assay_controls - found calls in:
   plate_validator_functions.R:1074
FALSE POSITIVE: prepare_batch_antigen_family - found calls in:
   plate_validator_functions.R:1117
FALSE POSITIVE: prepare_planned_visits - found calls in:
   plate_validator_functions.R:1141
FALSE POSITIVE: prepare_batch_header - found calls in:
   plate_validator_functions.R:1207
FALSE POSITIVE: filename - found calls in:
   import_lumifile.R:2052
   segment_reader.R:913
FALSE POSITIVE: download_bead_count_data - found calls in:
   bead_count_analysis_ui.R:307
   bead_count_analysis_ui.R:640
FALSE POSITIVE: shiny_notify - found calls in:
   std_curver_summary_ui.R:306
   std_curver_summary_ui.R:642
   std_curver_summary_ui.R:956
  ... and 6 more
FALSE POSITIVE: upsert_best_curve - found calls in:
   std_curver_summary_ui.R:301
   std_curver_summary_ui.R:637
   std_curver_summary_ui.R:951
  ... and 6 more
FALSE POSITIVE: build_upsert_sql_glue - found calls in:
   db_functions.R:267
FALSE POSITIVE: fetch_best_pred_all - found calls in:
   std_curver_summary_ui.R:39
   std_curver_summary_ui.R:390
   std_curver_summary_ui.R:726
FALSE POSITIVE: fetch_best_standard_all - found calls in:
   std_curver_summary_ui.R:29
   std_curver_summary_ui.R:380
   std_curver_summary_ui.R:716
FALSE POSITIVE: fetch_best_glance_all - found calls in:
   std_curver_summary_ui.R:744
FALSE POSITIVE: fetch_best_sample_se_all - found calls in:
   load_previous_stored_data.R:426
   std_curver_summary_ui.R:62
   std_curver_summary_ui.R:413
  ... and 1 more
FALSE POSITIVE: fetch_current_sc_options_wide - found calls in:
   std_curver_summary_ui.R:127
   std_curver_summary_ui.R:478
   std_curver_summary_ui.R:813
FALSE POSITIVE: calculate_sample_concentration_status - found calls in:
   dilution_analysis_ui.R:351
   dilution_analysis_ui.R:463
   dilution_analysis_ui.R:480
  ... and 3 more
FALSE POSITIVE: join_sample_standard_data - found calls in:
   dilution_analysis_ui.R:380
FALSE POSITIVE: plot_patient_dilution_series - found calls in:
   dilution_analysis_ui.R:828
FALSE POSITIVE: download_processed_dilution_data - found calls in:
   dilution_analysis_ui.R:960
FALSE POSITIVE: classify_sample - found calls in:
   dilution_linearity_functions.R:1515
FALSE POSITIVE: download_classified_sample - found calls in:
   dilution_analysis_ui.R:935
FALSE POSITIVE: download_dilution_contigency_summary_fun - found calls in:
   dilution_analysis_ui.R:483
FALSE POSITIVE: preserve_all_au - found calls in:
   dilution_analysis_ui.R:851
   dilution_analysis_ui.R:867
FALSE POSITIVE: preserve_passing_au - found calls in:
   dilution_analysis_ui.R:855
FALSE POSITIVE: geometric_mean_all_au_2 - found calls in:
   dilution_analysis_ui.R:859
FALSE POSITIVE: geometric_mean_passing_au_2 - found calls in:
   dilution_analysis_ui.R:863
FALSE POSITIVE: geometric_mean_positive_controls - found calls in:
   dilution_analysis_ui.R:875
FALSE POSITIVE: geometric_mean_blanks - found calls in:
   dilution_analysis_ui.R:880
FALSE POSITIVE: save_average_au - found calls in:
   dilution_analysis_ui.R:916
FALSE POSITIVE: is_optimization_plate - found calls in:
   import_lumifile.R:1453
FALSE POSITIVE: is_optimization_experiment_parsed - found calls in:
   ui_handler.R:563
FALSE POSITIVE: split_optimization_single_upload - found calls in:
   import_lumifile.R:1868
FALSE POSITIVE: split_optimization_plates - found calls in:
   ui_handler.R:575
FALSE POSITIVE: plot_single_regres - found calls in:
   dilution_linearity_functions.R:3591
FALSE POSITIVE: dilution_lm_facet - found calls in:
   dilution_linearity_functions.R:3625
FALSE POSITIVE: produce_all_plate_facets - found calls in:
   dilution_analysis_ui.R:1044
   dilutional_linearity_ui.R:281
FALSE POSITIVE: reloadReactive - found calls in:
   segment_reader.R:1262
   segment_reader.R:1444
   segment_reader.R:1591
  ... and 5 more
FALSE POSITIVE: generate_header - found calls in:
   segment_reader.R:1246
   segment_reader.R:1416
   segment_reader.R:1565
  ... and 1 more
FALSE POSITIVE: process_samples_with_class - found calls in:
   helpers.R:609
   xPonentReader.R:139
FALSE POSITIVE: createDownloadHandler - found calls in:
   load_previous_stored_data.R:496
   load_previous_stored_data.R:497
   load_previous_stored_data.R:498
  ... and 3 more
FALSE POSITIVE: check_nsample_plate - found calls in:
   load_previous_stored_data.R:745
   load_previous_stored_data.R:750
   load_previous_stored_data.R:755
  ... and 1 more
FALSE POSITIVE: validate_plate_data - found calls in:
   load_previous_stored_data.R:528
FALSE POSITIVE: checkExistingValueTypes - found calls in:
   outlier_ui1.R:235
FALSE POSITIVE: preLoadData - found calls in:
   outlier_ui1.R:267
FALSE POSITIVE: log_msg - found calls in:
   outlier_ui1.R:456
   outlier_ui1.R:457
   outlier_ui1.R:458
  ... and 9 more
FALSE POSITIVE: check_type_column - found calls in:
   plate_validator_functions.R:1375
FALSE POSITIVE: check_sample_description - found calls in:
   plate_validator_functions.R:1379
FALSE POSITIVE: check_standard_description - found calls in:
   plate_validator_functions.R:1384
FALSE POSITIVE: validate_batch_plate_metadata - found calls in:
   import_lumifile.R:2483
   import_lumifile.R:2750
   import_lumifile.R:2994
FALSE POSITIVE: validate_batch_bead_array_data - found calls in:
   import_lumifile.R:2489
   import_lumifile.R:2756
   import_lumifile.R:3000
FALSE POSITIVE: check_agg_bead_column - found calls in:
   plate_validator_functions.R:1389
FALSE POSITIVE: validate_required_columns - found calls in:
   plate_validator_functions.R:942
FALSE POSITIVE: get_null_rows - found calls in:
   plate_validator_functions.R:947
FALSE POSITIVE: check_existing_plates - found calls in:
   plate_validator_functions.R:1195
FALSE POSITIVE: get_existing_antigens - found calls in:
   plate_validator_functions.R:1118
FALSE POSITIVE: get_existing_visits - found calls in:
   plate_validator_functions.R:1142
FALSE POSITIVE: insert_new_rows - found calls in:
   plate_validator_functions.R:1120
   plate_validator_functions.R:1144
FALSE POSITIVE: insert_to_table - found calls in:
   plate_validator_functions.R:1094
   plate_validator_functions.R:1208
FALSE POSITIVE: upload_specimen_data - found calls in:
   plate_validator_functions.R:1235
   plate_validator_functions.R:1253
   plate_validator_functions.R:1268
  ... and 1 more
FALSE POSITIVE: upload_antigen_family - found calls in:
   plate_validator_functions.R:1298
FALSE POSITIVE: upload_planned_visits - found calls in:
   plate_validator_functions.R:1306
FALSE POSITIVE: upload_batch_to_database - found calls in:
   import_lumifile.R:3183
FALSE POSITIVE: plate_validation - found calls in:
   import_lumifile.R:1788
FALSE POSITIVE: createValidateBadge - found calls in:
   import_lumifile.R:982
   import_lumifile.R:985
FALSE POSITIVE: createValidateBatchBadge - found calls in:
   import_lumifile.R:1226
   import_lumifile.R:1229
   import_lumifile.R:1235
FALSE POSITIVE: createUploadedBatchBadge - found calls in:
   import_lumifile.R:1302
   import_lumifile.R:1345
FALSE POSITIVE: createOptimizedBadge - found calls in:
   import_lumifile.R:1021
   import_lumifile.R:1039
FALSE POSITIVE: plot_model_comparisons - found calls in:
   std_curver_ui.R:452
FALSE POSITIVE: plot_standard_curve - found calls in:
   std_curver_ui.R:569
FALSE POSITIVE: error - found calls in:
   app.R:238
   app.R:261
   app.R:519
FALSE POSITIVE: scale_pcov - found calls in:
   std_curve_functions.R:2167
FALSE POSITIVE: summarize_sc_fits_plotly - found calls in:
   std_curver_summary_ui.R:252
   std_curver_summary_ui.R:588
   std_curver_summary_ui.R:902
FALSE POSITIVE: conduct_linear_interpolation - found calls in:
   std_curver_summary_functions.R:687
FALSE POSITIVE: compute_aggregated_curves - found calls in:
   std_curver_summary_ui.R:277
   std_curver_summary_ui.R:613
   std_curver_summary_ui.R:927
FALSE POSITIVE: conduct_linear_interpolation_batch - found calls in:
   std_curver_summary_ui.R:287
   std_curver_summary_ui.R:623
   std_curver_summary_ui.R:937
FALSE POSITIVE: summarise_by_timeperiod - found calls in:
   study_overview_ui.R:531
   study_overview_ui.R:565
   study_overview_ui.R:607
FALSE POSITIVE: make_interplate_summ_spec - found calls in:
   study_overview_ui.R:710
   study_overview_ui.R:774
FALSE POSITIVE: get_bg_color - found calls in:
   study_overview_ui.R:273
FALSE POSITIVE: select_safe - found calls in:
   study_overview_functions.R:1579
   study_overview_functions.R:1587
FALSE POSITIVE: make_timeperiod_grid - found calls in:
   study_overview_ui.R:548
   study_overview_ui.R:628
   study_overview_ui.R:653
FALSE POSITIVE: make_cv_scatterplot - found calls in:
   study_overview_ui.R:729
FALSE POSITIVE: plot_preped_analyte_fit_summary - found calls in:
   study_overview_ui.R:1235
   study_overview_ui.R:1245
FALSE POSITIVE: make_antigen_plate_bead - found calls in:
   study_overview_ui.R:901
   study_overview_ui.R:938
FALSE POSITIVE: plot_analyte_plate_specimen - found calls in:
   study_overview_ui.R:615
   study_overview_ui.R:677
FALSE POSITIVE: plot_inter_intra_cv - found calls in:
   study_overview_ui.R:742
   study_overview_ui.R:751
FALSE POSITIVE: plot_bead_count_summary_plot - found calls in:
   study_overview_ui.R:943
   study_overview_ui.R:993
   study_overview_ui.R:1006
  ... and 3 more
FALSE POSITIVE: plot_analyte_plate_model - found calls in:
   study_overview_ui.R:1252
   study_overview_ui.R:1262
FALSE POSITIVE: get_analyte_plate_proportion - found calls in:
   study_overview_ui.R:1287
   study_overview_ui.R:1296
FALSE POSITIVE: download_first_visit_class - found calls in:
   subgroup_detection_ui.R:518
FALSE POSITIVE: download_visit_difference - found calls in:
   subgroup_detection_ui.R:570
FALSE POSITIVE: download_difference_histogram_data - found calls in:
   subgroup_detection_ui.R:620
FALSE POSITIVE: download_assay_classification_data - found calls in:
   subgroup_detection_ui.R:670
FALSE POSITIVE: reset_import_values - found calls in:
   ui_handler.R:774
FALSE POSITIVE: get_user_projects - found calls in:
   ui_handler.R:303
   ui_handler.R:345
FALSE POSITIVE: get_user_projects_non_owner - found calls in:
   ui_handler.R:326
   ui_handler.R:351
FALSE POSITIVE: load_project - found calls in:
   ui_handler.R:306
   ui_handler.R:329

=== FINAL RESULTS =========================================
Initially flagged as dead: 187
Confirmed false positives: 115
Truly stranded functions : 72

TRULY STRANDED FUNCTIONS:
  - collect_function_definitions  (find_stranded.R:35)
  - find_reactive_entries  (find_stranded.R:141)
  - is_likely_entry_point  (find_stranded.R:171)
  - shinyValue  (app.R:787)
  - add_clean_plate_id_from_filename  (batch_layout_functions.R:425)
  - add_clean_plate_id_from_plate_filename  (batch_layout_functions.R:432)
  - apply_default_values_to_plates_map  (batch_layout_functions.R:656)
  - validate_batch_description  (batch_layout_functions.R:823)
  - process_uploaded_files_v2  (batch_layout_functions.R:919)
  - content  (bead_count_analysis_ui.R:318)
  - pg_type_map  (db_functions.R:495)
  - fetch_best_tidy_all  (db_functions.R:670)
  - content  (dilution_analysis_ui.R:495)
  - content  (dilution_analysis_ui.R:944)
  - content  (dilution_analysis_ui.R:970)
  - get_dilution_parameters  (dilution_linearity_functions.R:2)
  - fetch_sample_data_linearity  (dilution_linearity_functions.R:10)
  - classify_sample_data  (dilution_linearity_functions.R:820)
  - parse_leaf_path  (dilution_linearity_functions.R:1475)
  - plot_classification_heatmap  (dilution_linearity_functions.R:1576)
  - obtain_passing_subject_contigency  (dilution_linearity_functions.R:1627)
  - obtain_contigency_table  (dilution_linearity_functions.R:1653)
  - obtain_passing_patients  (dilution_linearity_functions.R:1838)
  - obtain_passing_dilutions_df  (dilution_linearity_functions.R:1849)
  - compute_average_au  (dilution_linearity_functions.R:1890)
  - fit_model  (dilution_linearity_functions.R:2859)
  - fit_model_xy  (dilution_linearity_functions.R:2898)
  - fit_raw_assay_response_model  (dilution_linearity_functions.R:2979)
  - content  (dilutional_linearity_ui.R:501)
  - content  (dilutional_linearity_ui.R:520)
  - content  (dilutional_linearity_ui.R:543)
  - createLink  (helpers.R:2)
  - map_data_types  (helpers.R:422)
  - calculate_x_min_max_dynamic  (helpers.R:648)
  - welsch.weight  (helpers.R:668)
  - compute_allplate_robust_curves  (helpers.R:835)
  - createDownloadPlotData  (helpers.R:984)
  - content  (helpers.R:989)
  - content  (import_lumifile.R:878)
  - content  (load_previous_stored_data.R:9)
  - d2xY5  (model_functions.R:13)
  - d2xY4  (model_functions.R:32)
  - d2xYd5  (model_functions.R:48)
  - d2xYd4  (model_functions.R:66)
  - d2xYgomp4  (model_functions.R:80)
  - dxdyiY5  (model_functions.R:94)
  - dxdyiYd5  (model_functions.R:111)
  - dxdyiY4  (model_functions.R:148)
  - dxdyiYd4  (model_functions.R:161)
  - dxdyiYgomp4  (model_functions.R:174)
  - hidden  (outlier_ui1.R:1)
  - content  (outlier_ui1.R:612)
  - content  (outlier_ui1.R:851)
  - onFulfilled  (outlier_ui1.R:1157)
  - content  (outliers.R:1077)
  - validate_batch_plate  (plate_validator_functions.R:94)
  - validate_batch_bead_array  (plate_validator_functions.R:281)
  - debug_sample_preparation  (plate_validator_functions.R:785)
  - glance_plot_data  (plot_functions.R:601)
  - make_feature_plot  (plot_functions.R:626)
  - produce_buffer_controls_data  (plot_norm_functions.R:35)
  - produce_controls_data  (plot_norm_functions.R:53)
  - load_norm_plate_data  (plot_norm_functions.R:60)
  - grad_inv_Y5_fixed_robust  (se_x_robust_fix.R:151)
  - grad_inv_Yd5_fixed_robust  (se_x_robust_fix.R:161)
  - grad_y_Y5_fixed_robust  (se_x_robust_fix.R:170)
  - grad_y_Yd5_fixed_robust  (se_x_robust_fix.R:178)
  - propagate_error_analytic_robust  (se_x_robust_fix.R:361)
  - parse_plate_header  (segment_reader.R:876)
  - compute_log_response  (std_curve_functions.R:373)
  - obtain_response_variable  (std_curve_functions.R:449)
  - interpolate_vertex  (std_curve_functions.R:974)
  - calculate_predicted_concentration  (std_curve_functions.R:1234)
  - test_se_at_inflection  (std_curve_functions.R:2340)
  - aggregate_params  (std_curver_summary_functions.R:1)
  - content  (std_curver_summary_ui.R:332)
  - content  (std_curver_ui.R:582)
  - content  (std_curver_ui.R:603)
  - content  (std_curver_ui.R:663)
  - obtain_all_sc_source  (study_configuration.R:32)
  - content  (study_configuration_ui.R:1620)
  - gn  (study_overview_functions.R:11)
  - summarise_by_plate_id  (study_overview_functions.R:640)
  - pivot_by_plate  (study_overview_functions.R:1460)
  - pivot_sample_col  (study_overview_functions.R:1466)
  - report_vars  (study_overview_functions.R:1473)
  - content  (study_overview_ui.R:590)
  - content  (study_overview_ui.R:675)
  - content  (study_overview_ui.R:749)
  - content  (study_overview_ui.R:763)
  - content  (study_overview_ui.R:992)
  - content  (study_overview_ui.R:1019)
  - content  (study_overview_ui.R:1260)
  - content  (study_overview_ui.R:1274)
  - content  (study_overview_ui.R:1285)
  - content  (study_overview_ui.R:1344)
  - content  (study_overview_ui.R:1383)
  - content  (subgroup_detection_summary_ui.R:433)
  - content  (subgroup_detection_ui.R:530)
  - content  (subgroup_detection_ui.R:584)
  - content  (subgroup_detection_ui.R:631)
  - content  (subgroup_detection_ui.R:685)
  - signed_euclidean  (subgroup_function.R:35)
  - grouped_antigens  (subgroup_summary_functions.R:132)
  - get_max_value  (subgroup_summary_functions.R:189)

Done.