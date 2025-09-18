% ========================================================================
% TERNPLOT - Ternary Phase Diagram Plotting Function
% ========================================================================
%
% DESCRIPTION:
%   Creates ternary phase diagrams for three-component systems. Automatically
%   normalizes data if not already in fractional form and provides flexible
%   plotting options compatible with standard MATLAB plot functions.
%
% SYNTAX:
%   TERNPLOT(A, B)                    % C calculated as 1 - A - B
%   TERNPLOT(A, B, C)                 % Three components specified
%   TERNPLOT(A, B, C, LINETYPE)       % With specified line type
%   TERNPLOT(..., 'Parameter', Value) % With additional parameters
%
% INPUTS:
%   A, B, C - Component values (vectors or scalars)
%             If not fractions, automatically normalized by total
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
%   If input values are not fractions (sum ≠ 1), they are normalized:
%   fA = A / (A + B + C)
%   fB = B / (A + B + C)
%   fC = C / (A + B + C)
%
% FEATURES:
%   - Automatic data normalization
%   - Compatible with TITLE, LEGEND, HOLD commands
%   - Grid customization with 'majors' parameter
%   - Works with TERNLABEL for axis labeling
%   - Based on POLAR plotting methodology
%
% EXAMPLE:
%   A = [0.1, 0.3, 0.6];  % Fractional values
%   B = [0.2, 0.4, 0.2];
%   C = [0.7, 0.3, 0.2];
%   ternplot(A, B, C, 'ro', 'MarkerSize', 8);
%   ternlabel('Component A', 'Component B', 'Component C');
%   title('Ternary Phase Diagram');
%
% DEPENDENCIES:
%   - fractions() function (for normalization)
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
% REPOSITORY: PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
%
% PACKAGE: Carl Sandrock's ternplot package
% REPOSITORY: https://github.com/alchemyst/ternplot
% CITATION: Sandrock, C. (2025). alchemyst/ternplot. GitHub. Retrieved September 18, 2025.
%
% SEE ALSO: TERNLABEL, TERNPLOTA, FRACTIONS, PLOT, POLAR
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

function handles = ternplot(A, B, C, varargin)

if nargin < 3
    C = 1 - (A+B);
end;

[varargin, majors] = extractpositional(varargin, 'majors', 10);

[fA, fB, fC] = fractions(A, B, C);

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