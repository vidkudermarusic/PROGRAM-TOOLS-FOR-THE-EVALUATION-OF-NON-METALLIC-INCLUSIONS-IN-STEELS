% AUTHOR: Vid Kuder Marušič
% EMAIL: vidkm30@gmail.com
% DATE: 2025
% VERSION: 1.0.0
% LICENSE: MIT License
% COMPATIBILITY: MATLAB R2018b or later
% REPOSITORY: https://github.com/vidkudermarusic/PROGRAM-TOOLS-FOR-THE-EVALUATION-OF-NON-METALLIC-INCLUSIONS-IN-STEELS
%
% ========================================================================
% DYNAMIC_TERNARY_ANALYZER - Interactive Ternary Plotting Analyzer
% ========================================================================
%
% DESCRIPTION:
%   Interactive MATLAB script for creating ternary plots from Excel data.
%   Allows dynamic column selection, dual normalization methods, and 
%   customizable plotting options with high-resolution export capabilities.
%
% FEATURES:
%   - Dynamic Excel file loading and column selection
%   - Support for multiple columns per component (automatically summed)
%   - Dual normalization methods: fractions() and fractionsA()
%   - Customizable marker styles, sizes, and colors
%   - High-resolution PNG export (600 DPI)
%   - Interactive user interface with validation
%   - Real-time data preview and analysis summary
%
% DEPENDENCIES:
%   REQUIRED: Carl Sandrock's ternplot package
%   Repository: https://github.com/alchemyst/ternplot
%   Citation: Sandrock, C. (2025). alchemyst/ternplot. GitHub. Retrieved September 18, 2025.
%   
%   MATLAB Packages:
%   - statistics package (pkg install -forge statistics)
%   - io package (pkg install -forge io)
%
% USAGE:
%   Run the script and follow interactive prompts:
%   1. Select Excel file (.xlsx or .xls)
%   2. Choose columns for components A, B, C
%   3. Select marker style and size
%   4. Choose normalization method
%   5. Save plot if desired
%
% INPUT REQUIREMENTS:
%   - Excel files with headers in first row
%   - Numerical data in selected columns
%   - At least 3 columns with valid data
%
% OUTPUT:
%   - Interactive ternary plots
%   - High-resolution PNG files (optional)
%   - Analysis summary with statistics
%
% NORMALIZATION METHODS:
%   1. fractions(): A(rel) = A/(A+B+C) - Relative proportions
%   2. fractionsA(): A(wt%) = A/100 - Percentage-based
%   3. Both: Overlay comparison of both methods
%
% EXAMPLE:
%   run('DYNAMIC_TERNARY_ANALYZER.m')
%   % Follow interactive prompts to create ternary plots
%
% TROUBLESHOOTING:
%   - Ensure Carl Sandrock's ternplot package is installed
%   - Check Excel file format and data structure
%   - Verify MATLAB packages are loaded
%
% ========================================================================

clear all;
close all;
clc;

% Load packages
pkg load statistics;
pkg load io;

fprintf('=== DYNAMIC TERNARY PLOTTING ANALYZER ===\n\n');

% Function to get Excel file and column information
function [data, headers, selected_cols, component_names] = get_user_selections()
    % Get Excel file from user
    [filename, pathname] = uigetfile({'*.xlsx;*.xls', 'Excel Files (*.xlsx, *.xls)'}, ...
                                   'Select Excel file for analysis');
    if filename == 0
        error('No file selected. Exiting.');
    end

    fullpath = fullfile(pathname, filename);
    fprintf('Selected file: %s\n', fullpath);

    % Read both headers and data in ONE operation for efficiency
    fprintf('Loading data...\n');
    [data, ~, raw_data] = xlsread(fullpath, 'Sheet1');
    headers = raw_data(1, :);
    fprintf('Data loaded successfully! (%d columns, %d rows)\n', size(data, 2), size(data, 1));

    % Display ALL available columns (essential for user decision making)
    fprintf('\nAvailable columns:\n');
    for i = 1:length(headers)
        if ~isempty(headers{i}) && ischar(headers{i})
            fprintf('  %2d: %s\n', i, headers{i});
        else
            fprintf('  %2d: [Empty/Non-text]\n', i);
        end
    end

    % Get user selections for components with multiple column support
    fprintf('\n=== COLUMN SELECTION ===\n');
    fprintf('Select columns for ternary analysis:\n');
    fprintf('You can select multiple columns per component (they will be summed)\n');
    fprintf('Enter column numbers separated by spaces (e.g., 5 6 7 for multiple columns)\n\n');

    selected_cols = cell(1, 3);  % Changed to cell array to store multiple columns
    component_names = cell(1, 3);

    for i = 1:3
        while true
            col_input = input(sprintf('Enter column numbers for Component %s: ', char('A' + i - 1)), 's');
            if isempty(col_input)
                fprintf('Please enter at least one column number.\n');
                continue;
            end

            % Parse column numbers
            col_nums = str2num(col_input);
            if isempty(col_nums)
                fprintf('Invalid input. Please enter numbers separated by spaces.\n');
                continue;
            end

            % Validate column numbers
            if any(col_nums < 1) || any(col_nums > length(headers))
                fprintf('Column numbers out of range. Valid range: 1-%d\n', length(headers));
                continue;
            end

            % Check if any selected columns are empty
            empty_cols = false;
            for j = 1:length(col_nums)
                if isempty(headers{col_nums(j)})
                    empty_cols = true;
                    break;
                end
            end

            if empty_cols
                fprintf('One or more selected columns are empty. Please choose different columns.\n');
                continue;
            end

            % Store selected columns and create component name
            selected_cols{i} = col_nums;

            if length(col_nums) == 1
                component_names{i} = headers{col_nums(1)};
                fprintf('Selected: Column %d - %s\n', col_nums(1), headers{col_nums(1)});
            else
                % Create combined name for multiple columns
                combined_name = '';
                for j = 1:length(col_nums)
                    if j == 1
                        combined_name = headers{col_nums(j)};
                    else
                        combined_name = [combined_name, ' + ', headers{col_nums(j)}];
                    end
                end
                component_names{i} = combined_name;
                fprintf('Selected: Columns %s - %s\n', num2str(col_nums), combined_name);
            end
            break;
        end
    end
end

% Function to extract and sum data from multiple columns
function component_data = extract_component_data(data, column_indices)
    if length(column_indices) == 1
        % Single column - just extract
        component_data = data(:, column_indices(1));
    else
        % Multiple columns - sum them
        component_data = sum(data(:, column_indices), 2);
    end
end

% Function to create dynamic ternary plots
function fig_handle = create_ternary_plots(A, B, C, component_names, plot_options)
    % Extract plot options
    marker_style = plot_options.marker_style;
    marker_size = plot_options.marker_size;
    line_width = plot_options.line_width;
    normalization_method = plot_options.normalization_method;
    plot_title = plot_options.plot_title;

    % Create figure with better naming and positioning
    fig_handle = figure('Name', plot_title, 'NumberTitle', 'off', 'Position', [100, 100, 1400, 1000]);
    clf;

    % Choose normalization method
    switch normalization_method
        case 'fractions'
            ternplot(A, B, C, marker_style, 'MarkerSize', marker_size, 'LineWidth', line_width);
            norm_label = 'A(rel)=A/(A+B+C)';
        case 'fractionsA'
            ternplotA(A, B, C, marker_style, 'MarkerSize', marker_size, 'LineWidth', line_width);
            norm_label = 'A(wt%)=A/100';
        case 'both'
            ternplot(A, B, C, 'r.', 'MarkerSize', marker_size, 'LineWidth', line_width);
            hold on;
            ternplotA(A, B, C, 'b.', 'MarkerSize', marker_size, 'LineWidth', line_width);
            legend('fractions - A(rel)=A/(A+B+C)', 'fractionsA - A(wt%)=A/100', 'Location', 'northeast', 'Position', [0.92, 0.92, 0.08, 0.08]);
            norm_label = 'Dvojne točke';
        otherwise
            error('Unknown normalization method');
    end

    % Clean component names by removing (Wt%) and other common suffixes
    clean_names = clean_component_names(component_names);

    % Dynamic labels with better spacing
    ternlabel(clean_names{1}, clean_names{2}, clean_names{3});

    % Adjust label positions to be further from plot
    ax = gca;
    if isfield(ax, 'XLabel') && ~isempty(ax.XLabel)
        ax.XLabel.Position(2) = ax.XLabel.Position(2) * 1.3; % Move X label 30% further out
    end
    if isfield(ax, 'YLabel') && ~isempty(ax.YLabel)
        ax.YLabel.Position(1) = ax.YLabel.Position(1) * 1.3; % Move Y label 30% further out
    end

    % Create clean plot title without suffixes
    clean_title = sprintf('Ternary Analysis: %s vs %s vs %s', clean_names{1}, clean_names{2}, clean_names{3});
    title(sprintf('%s\n(%s)', clean_title, norm_label));

    % Enhanced styling
    grid on;
    set(gca, 'FontSize', 11, 'LineWidth', 1.2);
    set(gcf, 'Color', 'white');

    % Add data point count to title
    title(sprintf('%s\n(%s) - %d data points', clean_title, norm_label, length(A)));
end

% Function to clean component names
function clean_names = clean_component_names(component_names)
    clean_names = cell(size(component_names));
    for i = 1:length(component_names)
        name = component_names{i};
        % Remove common suffixes and parentheses
        name = regexprep(name, '\s*\([^)]*\)', '');  % Remove (Wt%), (%), etc.
        name = regexprep(name, '\s*\[[^\]]*\]', ''); % Remove [content]
        name = strtrim(name);                         % Remove extra spaces
        clean_names{i} = name;
    end
end

% Function to save plots
function save_plot(plot_title, component_names, fig_handle)
    fprintf('\n=== PLOT SAVING ===\n');
    save_choice = input('Save this plot? (y/n): ', 's');

    if strcmpi(save_choice, 'y') || strcmpi(save_choice, 'yes')
        % Create filename from component names
        clean_names = clean_component_names(component_names);

        % Handle combined names (e.g., "SiO2 + Al2O3") for filename
        safe_names = cell(size(clean_names));
        for i = 1:length(clean_names)
            name = clean_names{i};
            % Replace + with plus for filename
            name = strrep(name, ' + ', '_plus_');
            % Replace spaces and other problematic characters
            name = strrep(name, ' ', '_');
            name = strrep(name, '/', '_');
            name = strrep(name, '\', '_');
            safe_names{i} = name;
        end

        filename = sprintf('ternary_%s_vs_%s_vs_%s.png', safe_names{1}, safe_names{2}, safe_names{3});

        % Debug information
        fprintf('Debug: Figure handle type: %s\n', class(fig_handle));
        fprintf('Debug: Figure handle value: %g\n', fig_handle);
        fprintf('Debug: ishandle result: %d\n', ishandle(fig_handle));

        % Try multiple approaches to save the plot
        success = false;

        % Method 1: Try using the provided handle
        if ishandle(fig_handle)
            try
                figure(fig_handle);
                print(filename, '-dpng', '-r600');
                fprintf('Plot saved as: %s\n', filename);
                success = true;
            catch
                fprintf('Method 1 failed: Could not save using provided handle\n');
            end
        end

        % Method 2: Try using current figure
        if ~success
            try
                fprintf('Trying to save current figure...\n');
                print(filename, '-dpng', '-r600');
                fprintf('Plot saved as: %s (using current figure)\n', filename);
                success = true;
            catch
                fprintf('Method 2 failed: Could not save current figure\n');
            end
        end

        % Method 3: Try to find any available figure
        if ~success
            try
                available_figures = findall(0, 'Type', 'figure');
                if ~isempty(available_figures)
                    fprintf('Found %d available figures, trying to save the first one...\n', length(available_figures));
                    figure(available_figures(1));
                    print(filename, '-dpng', '-r600');
                    fprintf('Plot saved as: %s (using available figure)\n', filename);
                    success = true;
                else
                    fprintf('No figures available for saving\n');
                end
            catch
                fprintf('Method 3 failed: Could not save any available figure\n');
            end
        end

        if ~success
            fprintf('Warning: All save methods failed. Plot could not be saved.\n');
            fprintf('Available figures: %s\n', num2str(findall(0, 'Type', 'figure')));
        end
    end
end

% Main execution
try
    % Get user selections
    [data, headers, selected_cols, component_names] = get_user_selections();

    % Extract selected columns (now supporting multiple columns per component)
    A = extract_component_data(data, selected_cols{1});
    B = extract_component_data(data, selected_cols{2});
    C = extract_component_data(data, selected_cols{3});

    % Remove any rows with NaN or missing values
    valid_rows = ~isnan(A) & ~isnan(B) & ~isnan(C);
    A = A(valid_rows);
    B = B(valid_rows);
    C = C(valid_rows);

    fprintf('\nSuccessfully loaded %d valid data points\n', length(A));

    % Display selected columns with better formatting
    fprintf('Selected columns:\n');
    for i = 1:3
        if length(selected_cols{i}) == 1
            fprintf('  Component %s: Column %d (%s)\n', char('A' + i - 1), selected_cols{i}(1), component_names{i});
        else
            fprintf('  Component %s: Columns %s (%s)\n', char('A' + i - 1), num2str(selected_cols{i}), component_names{i});
        end
    end
    fprintf('\n');

    % Enhanced plot configuration
    fprintf('\n=== PLOT CONFIGURATION ===\n');

    % Marker style selection with visual examples
    marker_options = {'.', 'o', 's', '^', 'v', 'd', 'p', 'h'};
    marker_names = {'Point', 'Circle', 'Square', 'Triangle Up', 'Triangle Down', 'Diamond', 'Plus', 'Hexagon'};
    fprintf('Available marker styles:\n');
    for i = 1:length(marker_options)
        fprintf('  %d: %s (%s)\n', i, marker_names{i}, marker_options{i});
    end

    while true
        marker_idx = input('Select marker style (1-8): ');
        if marker_idx >= 1 && marker_idx <= length(marker_options)
            marker_style = marker_options{marker_idx};
            fprintf('Selected: %s\n', marker_names{marker_idx});
            break;
        else
            fprintf('Please enter a number between 1 and 8.\n');
        end
    end

    % Other plot parameters with validation
    marker_size = input('Enter marker size (default 2): ');
    if isempty(marker_size) || marker_size <= 0
        marker_size = 2;
        fprintf('Using default marker size: 2\n');
    end

    line_width = input('Enter line width (default 1.5): ');
    if isempty(line_width) || line_width <= 0
        line_width = 1.5;
        fprintf('Using default line width: 1.5\n');
    end

    % Normalization method selection
    fprintf('\nNačin izračuna:\n');
    fprintf('  1: fractions() - A(rel)=A/(A+B+C)\n');
    fprintf('  2: fractionsA() - A(wt%%)=A/100\n');
    fprintf('  3: Both methods\n');

    while true
        norm_choice = input('Select normalization method (1-3): ');
        if norm_choice >= 1 && norm_choice <= 3
            norm_methods = {'fractions', 'fractionsA', 'both'};
            normalization_method = norm_methods{norm_choice};
            fprintf('Selected: %s\n', normalization_method);
            break;
        else
            fprintf('Please enter a number between 1 and 3.\n');
        end
    end

    % Create plots based on user selections
    fprintf('\n=== CREATING PLOTS ===\n');

    if strcmp(normalization_method, 'both')
        % Create comparison plot
        clean_names = clean_component_names(component_names);
        plot_title = sprintf('Ternary Analysis: %s vs %s vs %s', clean_names{1}, clean_names{2}, clean_names{3});
        plot_options = struct('marker_style', marker_style, 'marker_size', marker_size, ...
                            'line_width', line_width, 'normalization_method', 'both', ...
                            'plot_title', plot_title);

        fig_handle = create_ternary_plots(A, B, C, component_names, plot_options);
        fprintf('Comparison plot created successfully.\n');

        % Ensure figure stays open and is current
        figure(fig_handle);
        drawnow;
        pause(0.5); % Small delay to ensure figure is fully rendered

        % Save plot
        save_plot(plot_title, component_names, fig_handle);
    else
        % Create individual plots
        clean_names = clean_component_names(component_names);
        plot_title = sprintf('Ternary Analysis: %s vs %s vs %s', clean_names{1}, clean_names{2}, clean_names{3});
        plot_options = struct('marker_style', marker_style, 'marker_size', marker_size, ...
                            'line_width', line_width, 'normalization_method', normalization_method, ...
                            'plot_title', plot_title);

        fig_handle = create_ternary_plots(A, B, C, component_names, plot_options);
        fprintf('Ternary plot created successfully.\n');

        % Ensure figure stays open and is current
        figure(fig_handle);
        drawnow;
        pause(0.5); % Small delay to ensure figure is fully rendered

        % Save plot
        save_plot(plot_title, component_names, fig_handle);
    end

    % Enhanced normalization test
    fprintf('\n=== NORMALIZATION TEST ===\n');
    fprintf('Testing normalization for first data point:\n');
    fprintf('Raw values: %s=%.1f, %s=%.1f, %s=%.1f\n', ...
            component_names{1}, A(1), component_names{2}, B(1), component_names{3}, C(1));

    [fA1, fB1, fC1] = fractions(A(1), B(1), C(1));
    fprintf('fractions():  %s=%.3f, %s=%.3f, %s=%.3f, Sum=%.3f\n', ...
            component_names{1}, fA1, component_names{2}, fB1, component_names{3}, fC1, fA1+fB1+fC1);

    [fA2, fB2, fC2] = fractionsA(A(1), B(1), C(1));
    fprintf('fractionsA(): %s=%.3f, %s=%.3f, %s=%.3f, Sum=%.3f\n', ...
            component_names{1}, fA2, component_names{2}, fB2, component_names{3}, fC2, fA2+fB2+fC2);

    % Additional insights
    fprintf('\n=== ANALYSIS SUMMARY ===\n');
    fprintf('File processed: %d columns, %d rows\n', size(data, 2), size(data, 1));
    fprintf('Valid data points: %d\n', length(A));
    fprintf('Components analyzed: %s, %s, %s\n', component_names{1}, component_names{2}, component_names{3});
    fprintf('Normalization method: %s\n', normalization_method);
    fprintf('Plot created with %s markers, size %.1f\n', marker_names{marker_idx}, marker_size);

    fprintf('\n=== ANALYSIS COMPLETED ===\n');
    fprintf('Your ternary analysis has been completed successfully!\n');

catch ME
    fprintf('\nError occurred: %s\n', ME.message);
    fprintf('Stack trace:\n');
    for i = 1:length(ME.stack)
    fprintf('  %s (line %d)\n', ME.stack(i).name, ME.stack(i).line);
    end
    fprintf('\nTroubleshooting tips:\n');
    fprintf('  - Ensure your Excel file has headers in the first row\n');
    fprintf('  - Check that selected columns contain numerical data\n');
    fprintf('  - Verify that required packages are loaded (statistics, io)\n');
end
