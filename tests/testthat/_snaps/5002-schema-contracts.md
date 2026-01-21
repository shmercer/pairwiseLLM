# round_log_schema columns are stable

    c("round_id", "iter_at_refit", "n_items", "total_pairs", "new_pairs", 
    "batch_size", "window_W", "exploration_rate", "mean_degree", 
    "min_degree", "pos_balance_sd", "epsilon_mean", "epsilon_ci90_lo", 
    "epsilon_ci90_hi", "reliability_EAP", "theta_sd_median", "tau", 
    "theta_sd_pass", "U0", "U_top_median", "U_abs", "U_pass", 
    "U_dup_threshold", "hard_cap_reached", "hard_cap_threshold", 
    "n_unique_pairs_seen", "scheduled_pairs", "proposed_pairs", 
    "completed_pairs", "rank_stability_pass", "frac_weak_adj", 
    "min_adj_prob", "weak_adj_threshold", "weak_adj_frac_max", 
    "min_adj_prob_threshold", "min_new_pairs_for_check", "divergences", 
    "min_ess_bulk", "max_rhat", "diagnostics_pass", "stop_decision", 
    "stop_reason", "mode")

# item_summary_schema columns are stable

    c("ID", "theta_mean", "theta_sd", "theta_ci90_lo", "theta_ci90_hi", 
    "theta_ci95_lo", "theta_ci95_hi", "rank_mean", "rank_sd", "deg", 
    "posA_prop")

