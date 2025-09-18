% ========================================================================
% FRACTIONS - Ternary Data Normalization (Relative Proportions)
% ========================================================================
%
% DESCRIPTION:
%   Normalizes ternary data to relative proportions where the sum of all
%   components equals 1.0. This method is useful for compositional analysis
%   where relative proportions are more important than absolute values.
%
% SYNTAX:
%   [fA, fB, fC] = fractions(A, B, C)
%
% INPUTS:
%   A, B, C - Vectors or scalars containing the raw component values
%             Can be any units (weight %, atomic %, etc.)
%
% OUTPUTS:
%   fA, fB, fC - Normalized fractional values (0-1)
%                fA + fB + fC = 1.0 (always)
%
% NORMALIZATION METHOD:
%   fA = A / (A + B + C)
%   fB = B / (A + B + C)  
%   fC = 1 - (fA + fB)
%
% USE CASES:
%   - Compositional analysis where relative proportions matter
%   - Ternary phase diagrams
%   - Statistical analysis of compositional data
%   - Comparison of samples with different total concentrations
%
% EXAMPLE:
%   A = [10, 20, 30];  % Component A values
%   B = [20, 30, 40];  % Component B values  
%   C = [70, 50, 30];  % Component C values
%   [fA, fB, fC] = fractions(A, B, C);
%   % Result: fA + fB + fC = [1.0, 1.0, 1.0]
%
% DEPENDENCIES:
%   None (standalone function)
%
% AUTHOR: Vid Kuder Marušič
% EMAIL: vidkm30@gmail.com
% REPOSITORY: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
% DATE: 2025
% VERSION: 1.0.0
%
% SEE ALSO: fractionsA, ternplot
% ======================================================================== 

function [fA, fB, fC] = fractions(A, B, C)
Total = (A+B+C);
fA = A./Total;
fB = B./Total;
fC = 1-(fA+fB);
%fA=A/100;
%fB=B/100;
%fC=C/max(C);