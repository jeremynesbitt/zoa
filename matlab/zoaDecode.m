function r = zoaDecode(json)
% zoaDecode  Decode a Zoa structured-result JSON string.
%
%   r = zoaDecode(json)
%
% Calls jsondecode, then for any grid with encoding=="base64-f64-le"
% decodes the base64 data field into a double matrix of size [nx, ny].
%
% Input:
%   json  - char or string containing a Zoa result JSON payload
%
% Output:
%   r     - MATLAB struct matching the JSON schema; grid data arrays
%           are decoded from base64 into double matrices.

    r = jsondecode(json);

    % Decode grids (if any)
    if isfield(r, 'grids')
        gridNames = fieldnames(r.grids);
        for k = 1:numel(gridNames)
            g = r.grids.(gridNames{k});
            if isfield(g, 'encoding') && strcmp(g.encoding, 'base64-f64-le')
                rawBytes = matlab.net.base64decode(g.data);
                vals     = typecast(uint8(rawBytes), 'double');
                r.grids.(gridNames{k}).data = reshape(vals, [g.nx, g.ny]);
            end
        end
    end

end
