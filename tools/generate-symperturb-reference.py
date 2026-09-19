"""Generate numeric oracles with the user-supplied SymPerturb 0.1.0 reference.

Usage: PYTHONPATH=/path/to/SymPerturb/src python tools/generate-symperturb-reference.py
The R tests consume the saved numbers; Python is not an R package dependency.
"""
from pathlib import Path
import json
import numpy as np
import pandas as pd
from symperturb.analysis import SymPerturbAnalyzer
from symperturb.state import post_intervention_moments, expected_observed_mean
from symperturb.topology import propagation_potential, edge_block, node_block
from symperturb.scoring import minmax_0_100

out = Path(__file__).resolve().parents[1] / 'tests/testthat/fixtures'
rng = np.random.default_rng(8042)
z = rng.normal(size=(50, 5))
loadings = np.array([[1,.5,0,0,-.2],[.4,1,-.5,0,0],[0,-.2,1,.6,0],[.2,0,.5,1,.3],[0,.2,0,.4,1]])
x = z @ loadings.T * np.array([.7,1.3,.9,1.7,.6]) + [1.7,2.2,1.4,.3,3.7]
data = pd.DataFrame(x, columns=list('ABCDE'))
data.to_csv(out / 'symperturb-data.csv', index=False, float_format='%.17g')
# Re-read to use precisely the same serialized participant data in both languages.
data = pd.read_csv(out / 'symperturb-data.csv', float_precision='round_trip')
modules = dict(A='m1', B='m1', C='m2', D='m3', E='m3')
anchors = dict(A=.2, D=-.1)
weights = dict(A=.7, B=1.8, C=0, D=.4, E=1.2)
costs = dict(A=.1, B=.5, D=.3, E=.2)
shared = dict(sequence_length=3, sequence_pool=4, sequence_beam_width=9,
              sequence_cost_lambda=.15, vpps_weights={'breadth':.4,'combination_value':1.6})
configs = {
  'zero_effect_ties': dict(shared, bounds=None, state_map='scale_only', run_robustness_scenarios=False),
  'subset': dict(shared, bounds=[1,3], combination_partner_k=1, run_robustness_scenarios=False),
  'reference': dict(shared, bootstrap_replicates=4, bootstrap_top_k=2, random_seed=927),
  'unbounded': dict(shared, bounds=None, ridge=0, run_robustness_scenarios=False),
  'location_only': dict(shared, state_map='location_only', adjacency_normalization='row', run_robustness_scenarios=False),
  'scale_only': dict(shared, state_map='scale_only', propagation_absolute=False, run_robustness_scenarios=False),
  'independent': dict(shared, state_map='independent', mu_power=.7, sigma_power=1.4,
                      adjacency_normalization='spectral', combination_mode='positive',
                      dose_grid=[0,.2,.8], dose_efficiency_grid=[.2,.6], responsiveness_epsilon=.15,
                      run_robustness_scenarios=False),
}

def clean(v):
    if isinstance(v, pd.DataFrame): return clean(v.to_dict(orient='records'))
    if isinstance(v, np.ndarray): return clean(v.tolist())
    if isinstance(v, (np.integer, np.floating, np.bool_)): return clean(v.item())
    if isinstance(v, dict): return {k:clean(x) for k,x in v.items()}
    if isinstance(v, (list,tuple)): return [clean(x) for x in v]
    return v

cases = {}
for name, cfg in configs.items():
    candidates = ['B','D','E'] if name == 'subset' else list(data.columns)
    analyzer = SymPerturbAnalyzer(data, modules, anchors=anchors, symptom_weights=weights, costs=costs, config=cfg, candidate_targets=candidates)
    result = analyzer.run()
    net = analyzer.network
    moments = []
    for targets, alpha in [([0],0),([0],.5),([0],1),([0,3],.35),([0,1,2,3,4],1)]:
        post = analyzer._post(targets, alpha)
        moments.append(dict(targets=[data.columns[i] for i in targets], alpha=alpha,
                            mean=post.mean, covariance=post.covariance,
                            observed=expected_observed_mean(post.mean,post.covariance,analyzer.bounds)))
    cfgfull = analyzer.config
    topo = []
    for norm in ['raw','row','spectral']:
        for absolute in [True,False]:
            for q in [0,.8,1]:
                kws=dict(T=6,gamma=.45,use_absolute=absolute,normalization=norm)
                topo.append(dict(normalization=norm,absolute=absolute,q=q,
                   baseline=propagation_potential(net.adjacency,**kws),
                   edge=propagation_potential(edge_block(net.adjacency,0,1,q),**kws),
                   node=propagation_potential(node_block(net.adjacency,0,q),**kws)))
    # Match NumPy's exact resamples, rather than equating unrelated RNG seeds.
    B=cfgfull['bootstrap_replicates']
    idx = np.random.default_rng(cfgfull['random_seed']).integers(0,len(data),size=(B,len(data)))
    draws=[]
    for b, rows in enumerate(idx):
        sub=SymPerturbAnalyzer(data.iloc[rows].reset_index(drop=True),modules,anchors=anchors,symptom_weights=weights,costs=costs,
            config=dict(cfg,bootstrap_replicates=0,run_robustness_scenarios=False,sequence_length=0))
        scores,_,_=sub.target_scores()
        for _, r in scores.iterrows():
            draws.append(dict(replicate=b+1,target=r.target,vpps=r.vpps,rank=r['rank'],top_k=int(r['rank']<=cfgfull['bootstrap_top_k'])))
    cases[name]=dict(config=cfg, candidate_targets=candidates, network=dict(mu=net.mu,covariance=net.covariance,precision=net.precision,
                      adjacency=net.adjacency,partial_correlations=net.partial_correlations),
                    target_scores=result.target_scores,dose_response=result.dose_response,pair_scores=result.pair_scores,
                    robustness=result.robustness,bootstrap=result.bootstrap,sequence=result.sequence,
                    network_edges=result.network_edges,moments=moments,topology=topo,
                    bootstrap_indices=idx+1,bootstrap_draws=draws)

# Exact singular block, signed pair and near-constant minmax edge cases.
mu=np.array([1.,2.,3.]); cov=np.array([[1.,1.,.5],[1.,1.,.5],[.5,.5,2.]])
post=post_intervention_moments(mu,cov,[0,1],{0:.4,1:.7},anchors=np.array([.1,.2,.3]))
result=dict(reference_version='0.1.0',reference_commit='76dd4178b285b80beb69f14a642e84ed1cabc7a0',
            modules=modules,anchors=anchors,symptom_weights=weights,costs=costs,cases=cases,
            singular=dict(mu=mu,covariance=cov,post_mean=post.mean,post_covariance=post.covariance),
            minmax_inputs=[[2,2,2],[1,1.000001,1.000002],[-1,0,3]],
            minmax_outputs=[minmax_0_100(v) for v in [[2,2,2],[1,1.000001,1.000002],[-1,0,3]]])
(out/'symperturb-reference.json').write_text(json.dumps(clean(result),indent=2,allow_nan=False)+'\n')
print('Saved seven complete reference cases, moments, topology and shared bootstrap draws.')
