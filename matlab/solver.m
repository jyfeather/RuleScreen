clear all;
clc;
addpath(genpath('.'));

%% input from R
AP = csvread('./matlab/AP.csv');
AZ = csvread('./matlab/AZ.csv');
B = csvread('./matlab/B.csv');
[m_P, ~] = size(AP);
[m_Z, ~] = size(AZ);
[t, n] = size(B);

%% predefined parmeter
alpha = 5;          
beta = 2;          

%% LP solver
cvx_begin
    variable eps_P(m_P) nonnegative;
    variable eps_Z(m_Z) nonnegative;
    variable w(n) nonnegative;
    minimize(sum(eps_P) + sum(eps_Z) + alpha * norm(w, 1));
    subject to
        norm(B * w, 1) <= beta;
        AP * w + eps_P >= 1;
        AZ * w == eps_Z;
        w <= 1;
        eps_P <= 1;
cvx_end

%% L1 norm Group Lasso Solver
cvx_begin
    variable eps_P(m_P) nonnegative;
    variable eps_Z(m_Z) nonnegative;
    variable w(n) nonnegative;
    minimize(sum(eps_P) + sum(eps_Z));
    subject to
        norm(w, 1) <= alpha
        sum(B * w) <= beta;
        w <= sum(B' * v);
        AP * w + eps_P >= 1;
        AZ * w == eps_Z;
        w <= 1;
        eps_P <= 1;
cvx_end

%% overlapping group lasso with SLEP
A = [AP; AZ];
y = [repmat(1, [1, m_P]), repmat(0, [1, m_Z])]';
opts.G = find(B');
opts.ind = [[1, nnz(B(1,:)), 1]'];
for i = 2:t
    base = opts.ind(2,i-1);
    opts.ind = [opts.ind, [base+1, base+nnz(B(i,:)), 1]'];
end
[x, funVal, ValueL, res]=overlapping_LeastR(A, y, [alpha, beta], opts);

%% analysize results
rule = (round(w.*1000))./1000;
feature = (round((B * rule).*1000))./1000;
num_rule = nnz(rule);
num_feature = nnz(feature);
loc_w = find(w);
loc_feature = find(feature);