# Command Reference

Commands listed here are the new-style Zoa commands.
Most original KDP commands still work — see the original manual.

## File I/O

| Parameter | Description |
| --------- | ----------- |
| RES filename | Restores a lens system from filename.zoa in the Projects folder. Use RES macro:filename to load a file from the Macros folder. Subdirectories are supported: RES macro:Bentley/Bentley3p1 Both forward and backward slashes are accepted. |
| SAV [filename] | Saves the current lens system to filename.zoa in the Projects folder. If filename is omitted, saves as currlens.zoa in the temp folder. The .zoa extension is added automatically if not provided. |

## System Parameters

| Parameter | Description |
| --------- | ----------- |
| DIM K | Sets the lens system units. K can be M (millimetres), C (centimetres), or I (inches). |
| EPD X | Sets the entrance pupil diameter to X lens units. |
| TIT 'titletext' | Sets the title of the lens system to titletext. Enclose the title in single quotes if it contains spaces. |

## Surface Parameters

| Parameter | Description |
| --------- | ----------- |
| CUY Sk X | Sets the curvature (1/radius) on surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). Use CUY Sk UMY X to set a paraxial marginal ray angle solve. |
| GLA Sk name | Sets the glass at surface Sk to the named material. Searches through available glass catalogs for the name. Use a numeric nd,vd pair (e.g. 1.5,50) to specify a model glass. |
| RDY Sk X | Sets the radius of surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). X is the new radius value in current lens units. |
| RMD Sk TYPE | Sets the ray mode (surface type) of surface Sk. TYPE can be REFL (mirror), REFR (refracting, default), or TIR (total internal reflection surface). |
| STO [Sk] | Sets surface Sk as the aperture stop. Sk can be S0 (object), S1..SN, or Si (current surface). If Sk is omitted, uses the current surface pointer. |
| THI Sk X | Sets the thickness on surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). X is the new thickness value in current lens units. |

## Solves

| Parameter | Description |
| --------- | ----------- |
| PIM | Adds a paraxial image solve on the surface before the image surface. Automatically adjusts that surface's thickness to place the paraxial focus at the image plane. |
| RED X | Adds a thickness solve on the object surface (S0) to set a paraxial reduction (magnification) factor of X. |

## System Management

| Parameter | Description |
| --------- | ----------- |
| DEL PIM / DEL SOL CUY Sk / DEL CON id | Deletes a system element. DEL PIM removes the paraxial image solve. DEL SOL CUY Sk removes the curvature solve on surface Sk. DEL CON id removes the optimization constraint with the given ID number. |
| INS Sk | Inserts a new surface before surface Sk. Use range notation (Si..k) to insert multiple surfaces at once. |

