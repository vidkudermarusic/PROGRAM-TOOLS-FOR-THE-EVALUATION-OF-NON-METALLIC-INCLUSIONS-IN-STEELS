% ========================================================================
% FRACTIONSA - Ternary Data Normalization (Percentage-based)
% ========================================================================
%
% DESCRIPTION:
%   Normalizes ternary data by dividing each component by 100, assuming
%   the input data is in percentage format. This method preserves the
%   original percentage scale while converting to decimal format.
%
% SYNTAX:
%   [fA, fB, fC] = fractionsA(A, B, C)
%
% INPUTS:
%   A, B, C - Vectors or scalars containing percentage values (0-100)
%             Typically weight percentages or atomic percentages
%
% OUTPUTS:
%   fA, fB, fC - Normalized values (0-1)
%                fA = A/100, fB = B/100, fC = C/100
%
% NORMALIZATION METHOD:
%   fA = A / 100
%   fB = B / 100
%   fC = C / 100
%
% USE CASES:
%   - Converting percentage data to decimal format
%   - Ternary plots where percentage scale is important
%   - Analysis of weight percentage or atomic percentage data
%   - Direct conversion from analytical results
%
% EXAMPLE:
%   A = [10, 20, 30];  % Weight percentages
%   B = [20, 30, 40];  % Weight percentages
%   C = [70, 50, 30];  % Weight percentages
%   [fA, fB, fC] = fractionsA(A, B, C);
%   % Result: fA = [0.1, 0.2, 0.3], fB = [0.2, 0.3, 0.4], fC = [0.7, 0.5, 0.3]
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
% SEE ALSO: fractions, ternplotA
% ======================================================================== 

function [fA, fB, fC] = fractionsA(A, B, C)
%Total = (A+B+C);
%fA = A./Total;
%fB = B./Total;
%fC = 1-(fA+fB);
fA=A/100;
fB=B/100;
fC=C/100;