# Command Reference

Commands listed here are the new-style Zoa commands.
Most original KDP commands still work — see the original manual.

## File I/O

| Parameter | Description |
| --------- | ----------- |
| PRT file | Print the contents of a text file to the terminal. |
| RES file | Restore (load) a lens from a .zoa file (RES macro:file loads from a macro). |
| RESAUTO | Restore the most recently auto-saved lens. |
| SAV file | Save the current lens to a .zoa file. |
| SAVESESS file | Save the current session (lens plus settings) to a file. |
| ZOA2CV file | Export the current lens to a CODE V sequence (.seq) file. |
| ZOA2ZMX file | Export the current lens to a Zemax (.zmx) file. |

## System Parameters

| Parameter | Description |
| --------- | ----------- |
| DIM M\|C\|I | Set the lens units: M (mm), C (cm), or I (inches). |
| EPD X | Set the entrance pupil diameter to X. |
| IND | List the refractive indices of each surface at each wavelength. |
| TIT 'text' | Set the lens title. |

## Fields & Wavelengths

| Parameter | Description |
| --------- | ----------- |
| REF n | Set the reference (control) wavelength to index n. |
| WL w1 [w2 ...] | Set the system wavelengths (nm); up to 5 values. |
| WTF s1 [s2 ...] | Set the field weights. |
| WTW s1 [s2 ...] | Set the spectral (wavelength) weights. |
| XAN a1 [a2 ...] | Set the X field angles (degrees). |
| XOB h1 [h2 ...] | Set the X object heights. |
| YAN a1 [a2 ...] | Set the Y field angles (degrees). |
| YIM h1 [h2 ...] | Set the Y paraxial image heights (field specification). |
| YOB h1 [h2 ...] | Set the Y object heights. |

## Surface Parameters

| Parameter | Description |
| --------- | ----------- |
| ASP Sk | Make surface Sk an asphere type.  If in lens editor mode or when loading a lens will operate on the current surface |
| CUX Sk <qual> j | Sets an XZ (X-toric) curvature solve on surface Sk.  Qualifiers: AMX (aplanatic marginal, KDP APX), ACX (aplanatic chief, APCX), IMX j (angle of incidence, PIX), ICX j (PICX), UMX j (ray slope, PUX), UCX j (PUCX). |
| CUY Sk X  \|  CUY Sk <qual> j | Sets the YZ curvature (1/radius) on surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). A solve qualifier sets a curvature solve instead: AMY (aplanatic marginal, KDP APY), ACY (aplanatic chief, APCY), IMY j (angle of incidence, PIY), ICY j (PICY), UMY j (ray slope, PUY), UCY j (PUCY). |
| GLA Sk name | Sets the glass at surface Sk to the named material. Searches through available glass catalogs for the name. Use a numeric nd,vd pair (e.g. 1.5,50) to specify a model glass. |
| I Sk c1 c2 ... | Set the aspheric (4th..) coefficients of surface Sk.  $z = \dfrac{c r^2}{1+\sqrt{1-(1+k)c^2 r^2}}$. |
| INS Sk \| INS Si..j | Insert one or more new surfaces before surface k (or over the range i..j). |
| K Sk X | Set the conic constant of surface Sk to X. |
| RDY Sk X | Sets the radius of surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). X is the new radius value in current lens units. |
| RMD Sk REFL\|REFR\|TIR | Set the surface Sk mode: reflector, refractor (air), or TIR reflector. |
| S [rd th glass] | Advance to / add the next surface (optionally set radius, thickness, glass). |
| SI [rd th glass] | Select the image surface. |
| SLB Sk 'label' | Set a text label on surface Sk. |
| SO [rd th glass] | Select the object surface (S0). |
| SPH Sk | Make surface Sk spherical type.  If in lens edit mode, will set the current surface to spherical type |
| STO Sk | Make surface Sk the aperture stop. |
| STOP Sk | Make surface Sk the aperture stop (alias of STO). |
| THI Sk X  \|  THI Sk <qual> j | Sets the thickness on surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). X is the new thickness value in current lens units. A solve qualifier sets a thickness solve instead: HMY j (paraxial marginal height, KDP PY), HCY j (chief height, PCY), HMX/HCX for XZ. |

## Apertures

| Parameter | Description |
| --------- | ----------- |
| CIR [Sk] r | Set a circular clear aperture of radius r on surface Sk (CIR EDG for an edge aperture). |
| CLI | Check/refresh the clear apertures. |
| ETH Sk X | Set the edge thickness aperture used by the MNE/MAE general constraints. |

## Lens System Commands

| Parameter | Description |
| --------- | ----------- |
| BES | Find the best-focus image thickness (minimum RMS). |
| DEL PIM \| DEL SOL <verb> Sk \| DEL CON n \| DEL VIG | Delete user-defined system data: PIM/thickness solves, angle/curvature solves (DEL SOL CUY\|CUX\|THI Sk), a constraint (DEL CON n), or vignetting. |
| FLY Si..j | Flip (reverse) the given range of surfaces. |
| RED f | Set an object-thickness reduction solve for paraxial magnification f. |
| REDO | Redo the last undone lens change. |
| SCA EFL X | Scale the system (e.g. SCA EFL 50 scales to an EFL of 50). |
| UNDO | Undo the last lens change. |

## Optimization

| Parameter | Description |
| --------- | ----------- |
| AUT ; <constraints/operands> ; GO | Run local optimization; define operands and constraints inside the loop. |
| AUTUI | Open the optimizer setup window (merit operands, constraints, variables). |
| CCY Sk n \| CCY Si..j n | Set the YZ-curvature variable code on surface(s). |
| CHA n ; <value> | Change entry n inside the current UPD loop. |
| DCON n | Delete constraint/operand number n. |
| EFL = v \| EFL v [w] | Effective focal length merit entry (constraint with =, or operand with target v, weight w). |
| FRZ | Freeze all variables (clear optimization variable codes). |
| GLC Sk n | Set the glass variable code on surface(s). |
| IMC = v \| IMC v [w] | Image-clearance / distance merit entry. |
| IMP X | Set the optimizer improvement goal. |
| KC Sk n | Set the conic-constant variable code on surface(s). |
| LCON | List the current merit operands and constraints. |
| MAE X | Set the minimum edge air spacing (general constraint). |
| MNA X | Set the minimum axial air spacing (general constraint). |
| MNE X | Set the minimum element edge thickness (general constraint). |
| MNT X | Set the minimum element center thickness (general constraint). |
| MXT X | Set the maximum element center thickness (general constraint, inside AUT). |
| PTB = v \| PTB v [w] | Petzval-blur merit entry. |
| SAS = v \| SAS v [w] | Sagittal-astigmatism merit entry. |
| SPO v [w] | RMS spot-size operand (weighted objective term). |
| TAR ; ... ; GO | Enter the optimization target loop (set operand targets/weights). |
| TAS = v \| TAS v [w] | Tangential-astigmatism merit entry. |
| TCO = v \| TCO v [w] | Transverse-color merit entry. |
| THC Sk n \| THC Si..j n | Set the thickness variable code on surface(s). |
| TOW ; ... ; GO | Enter the tolerancing/operand-weighting loop. |
| UPD CON | Enter an update loop to edit a data set (e.g. UPD CON for constraints). |

## Analysis

| Parameter | Description |
| --------- | ----------- |
| CX | Print chief-ray X data. |
| CY | Print chief-ray Y data. |
| EVA name | Evaluate a merit operand by name and print its value. |
| FIO | Print the results of the first-order (paraxial) ray trace. |
| RAYREF | Compute the per-field reference rays (R1..R5). |
| RSI fi..k wi..k relApX relApY | Trace a single ray and report ray-intercept / OPD data. |
| THO | List the third-order (Seidel) aberration coefficients. |
| XOFF X | Set the chief-ray X offset for the wavefront calculation. |
| YOFF X | Set the chief-ray Y offset for the wavefront calculation. |
| ZRN WAV [fi] [wj] [zk] [dN] | Fit the OPD error to Zernike polynomials and print a 36-term coefficient table. |

## Plotting

| Parameter | Description |
| --------- | ----------- |
| FAN | Ray-fan plot (used within a plot loop). |
| FIE | Astigmatic field-curvature and distortion plot. |
| MTF | Modulation-transfer-function plot. |
| PLOTTHO | Third-order (Seidel) aberration bar chart. |
| PLTRMS | RMS wavefront/spot vs field plot. |
| PMA | Optical-path-difference (wavefront map) plot. |
| PSF | Point-spread-function plot. |
| RIM | Ray-aberration (rim-ray fan) plot. |
| VIE ; [settings] ; GO | Draw the lens layout (2D/3D system view). |
| ZERN_TST ; [SETZERNC ...] ; GO | Zernike-coefficient-vs-field plot. |

## Plot Settings

| Parameter | Description |
| --------- | ----------- |
| AZI a | Set the azimuth angle of the 3D lens view. |
| DRAWSF Sk | Set the last surface drawn in the lens view. |
| DRAWSI Sk | Set the first surface drawn in the lens view. |
| ELEV a | Set the elevation angle of the 3D lens view. |
| IFR x | Set the frequency interval for the MTF plot. |
| MFR x | Set the maximum frequency for the MTF plot. |
| NBR ELE Si..j | Set the surface range for the lens drawing. |
| NUMRAYS n | Set the number of rays drawn in the lens view. |
| ORIENT ... | Set the orientation (elevation/azimuth) of the lens view. |
| RMSDATA WAVE\|SPOT | Choose the data type for the RMS-vs-field plot. |
| SETDENS n | Set the sampling density for the active plot. |
| SETWV n | Set the wavelength index for the active plot. |
| SETZERNC 5..9 \| 9,16,25 | Set which Zernike terms the Zernike plot shows (range or list). |
| SSI x | Set the scale of the active plot. |

## Zoom / Multi-Configuration

There is minimal zoom support right now: it stores the commands that differentiate a configuration from the base config.

| Parameter | Description |
| --------- | ----------- |
| POS n | Switch the active configuration (zoom position) to n. |
| ZOO ... \| ZOO PIM | Define zoom (multi-configuration) data. |

## Utilities

| Parameter | Description |
| --------- | ----------- |
| ! text | Comment line (ignored); used in .zoa files. |
| EDI PREF | Open an editor (EDI PREF opens preferences). |
| FALLBACK | Report the legacy-command fallback tally (retirement telemetry). |
| SET <option> ... | Set a system option (e.g. SET CAP, SET VIG). |
| SUR | Surface data command. |
| TERM | Reset terminal output to the default view. |

