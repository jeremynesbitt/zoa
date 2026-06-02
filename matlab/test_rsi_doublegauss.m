% test_rsi_doublegauss.m
% ---------------------------------------------------------------------------
% End-to-end smoke test for the Zoa structured-JSON interface from MATLAB.
%
% It:
%   1. Starts a zoa_server (via ZoaClient, over JeroMQ),
%   2. Loads the Double Gauss lens from the bundled CODE V sequence file,
%   3. Pulls a single-ray trace (RSI) back as a structured result, and
%   4. Prints the per-surface ray data as a table.
%
% Prerequisites:
%   - buildHB/zoa_server must be built (ninja -C buildHB).
%   - A jeromq-<ver>.jar must sit in this matlab/ directory
%     (see README.md). ZoaClient adds it to the Java path automatically.
%
% Run from this folder:   >> test_rsi_doublegauss
% ---------------------------------------------------------------------------

zoa = ZoaClient();          % spawns zoa_server, connects, waits for PONG
cleanup = onCleanup(@() zoa.close());   % guarantees server shutdown on exit/error

% --- 1. Load the Double Gauss lens -------------------------------------------
% CV2PRG converts the CODE V .seq file into the current Zoa lens.
fprintf('Loading Double Gauss lens...\n');
zoa.cmd("cv2prg DoubleGauss.seq");

% --- 2. Trace a ray and pull the structured result --------------------------
% RSI f<field> w<wavelength> <relApeX> <relApeY>
%   field 1, wavelength 1, upper marginal ray (relApeY = 1.0) so the
%   per-surface heights are visibly non-zero.
fprintf('Tracing RSI f1 w1 0 1.0 (upper marginal ray)...\n\n');
r = zoa.analyze("RSI f1 w1 0 1.0");

% --- 3. Display ---------------------------------------------------------------
fprintf('Result type : %s\n', r.type);
fprintf('Field       : %s   Wavelength : %s\n', r.meta.field, r.meta.wavelength);
fprintf('relApeX/Y   : %s / %s\n\n', r.meta.relApeX, r.meta.relApeY);

tbl  = r.tables.ray;
cols = string(tbl.col_labels);     % e.g. ["X" "Y" "Z" "XANG" "YANG"]
rows = string(tbl.row_labels);     % e.g. ["S0" "S1" ... ]
data = tbl.data;                   % numeric matrix [nSurf x nCols]

% Header
fprintf('%-6s', 'SURF');
fprintf('%14s', cols{:});
fprintf('\n');

% Rows
for i = 1:size(data, 1)
    fprintf('%-6s', rows{i});
    fprintf('%14.6g', data(i, :));
    fprintf('\n');
end

% The decoded data is a normal MATLAB matrix, so you can use it directly, e.g.:
%   yIntercepts = data(:, strcmp(cols, "Y"));
fprintf('\nDone. ''r'' is in the workspace; e.g. r.tables.ray.data is a %dx%d matrix.\n', ...
        size(data, 1), size(data, 2));

% zoa.close() runs automatically via onCleanup.
