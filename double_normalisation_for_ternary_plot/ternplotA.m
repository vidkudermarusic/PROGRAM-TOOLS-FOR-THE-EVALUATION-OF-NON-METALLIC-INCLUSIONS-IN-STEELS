% ========================================================================
% TERNPLOTA - Ternary Phase Diagram Plotting Function (Percentage-based)
% ========================================================================
%
% DESCRIPTION:
%   Creates ternary phase diagrams for three-component systems using percentage-based
%   normalization. This function is similar to TERNPLOT but uses fractionsA() for
%   normalization, making it suitable for percentage data (0-100%).
%
% SYNTAX:
%   TERNPLOTA(A, B)                    % C calculated as 1 - A - B
%   TERNPLOTA(A, B, C)                 % Three components specified
%   TERNPLOTA(A, B, C, LINETYPE)       % With specified line type
%   TERNPLOTA(..., 'Parameter', Value) % With additional parameters
%
% INPUTS:
%   A, B, C - Component values (vectors or scalars)
%             Typically percentage values (0-100), normalized by dividing by 100
%   LINETYPE - Line style specification (see PLOT function)
%              Examples: 'r-', 'bo', 'g--', etc.
%
% PARAMETERS:
%   Parameter  Default  Description
%   ---------  -------  -----------
%   majors     10       Number of major grid intervals
%
% OUTPUTS:
%   handles - Plot handles (optional)
%
% NORMALIZATION:
%   Input values are normalized using percentage-based method:
%   fA = A / 100
%   fB = B / 100
%   fC = C / 100
%
% FEATURES:
%   - Percentage-based data normalization
%   - Compatible with TITLE, LEGEND, HOLD commands
%   - Grid customization with 'majors' parameter
%   - Works with TERNLABEL for axis labeling
%   - Based on POLAR plotting methodology
%
% EXAMPLE:
%   A = [10, 30, 60];  % Percentage values
%   B = [20, 40, 20];
%   C = [70, 30, 20];
%   ternplotA(A, B, C, 'bo', 'MarkerSize', 8);
%   ternlabel('Component A (%)', 'Component B (%)', 'Component C (%)');
%   title('Ternary Phase Diagram (Percentage-based)');
%
% DEPENDENCIES:
%   - fractionsA() function (for percentage normalization)
%   - terncoords() function (for coordinate conversion)
%   - ternaxes() function (for axis creation)
%   - extractpositional() function (for parameter parsing)
%
% ORIGINAL AUTHOR: Carl Sandrock (2002-08-27)
% MODIFICATIONS: 
%   - 2016-04-05 (SA) Added 'majors' input argument
%   - 2025 (VKM) Enhanced documentation and integration
% CURRENT AUTHOR: Vid Kuder Marušič
% EMAIL: vidkm30@gmail.com
% REPOSITORY: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
%
% PACKAGE: Carl Sandrock's ternplot package
% REPOSITORY: https://github.com/alchemyst/ternplot
% CITATION: Sandrock, C. (2025). alchemyst/ternplot. GitHub. Retrieved September 18, 2025.
%
% SEE ALSO: TERNLABEL, TERNPLOT, FRACTIONSA, PLOT, POLAR
%
% TERNARY DIAGRAM LAYOUT:
%       B
%      / \
%     /   \
%    C --- A 
% ========================================================================

%       b
%      / \
%     /   \
%    c --- a 

% Author: Carl Sandrock 20020827

% To do

% Modifications
% 20160405 (SA) Added an input argument 'major'

% Modifiers
% CS Carl Sandrock
% SA Shahab Afshari

function handles = ternplotA(A, B, C, varargin)

if nargin < 3
    C = 1 - (A+B);
end;

[varargin, majors] = extractpositional(varargin, 'majors', 10);

[fA, fB, fC] = fractionsA(A, B, C);

[x, y] = terncoords(fA, fB, fC);

% Sort data points in x order
[x, i] = sort(x);
y = y(i);

% Make ternary axes only if not already holding
hold_state = ishold;
if ~hold_state
    [hold_state, cax, next] = ternaxes(majors);
else
    cax = gca;
    next = get(cax, 'NextPlot');
end

% plot data
q = plot(x, y, varargin{:});
if nargout > 0
    handles = q;
end
if ~hold_state
    set(gca,'dataaspectratio',[1 1 1]), axis off; set(cax,'NextPlot',next);
end