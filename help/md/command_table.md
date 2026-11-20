# Command Reference

Commands listed here are the new-style Zoa commands.
Most original KDP commands still work — see the original manual.

## File I/O

| Parameter | Description |
| --------- | ----------- |
| PRT file | Print the contents of a text file to the terminal. |
| RES file | Restore (load) a lens from a .zoa file (RES macro:file loads from a macro). A companion .zin plot file with the same name is loaded too, if present. |
| RESAUTO | Restore the most recently auto-saved lens. A companion .zin plot file with the same name is loaded too, if present. |
| SAV file | Save the current lens to a .zoa file. A companion .zin plot file (same name, .zin extension) is written too when any plots are open. |
| SAVESESS file | Save the current session (lens plus settings) to a file. |
| WRL [filename] | Write the lens as CODE V commands (filename.seq); same as ZOA2CV. |
| ZOA2CV file | Export the current lens to a CODE V sequence (.seq) file. |
| ZOA2ZMX file | Export the current lens to a Zemax (.zmx) file. |

## System Parameters

| Parameter | Description |
| --------- | ----------- |
| AFC [Yes\|No] | Afocal (Yes) or focal (No) evaluation mode; bare AFC shows the mode. |
| DDR [EDG f \| ?] | Default drawing rules: EDG f = default edge aperture as f times the clear aperture. |
| DIM M\|C\|I | Set the lens units: M (mm), C (cm), or I (inches). |
| EPD X | Set the entrance pupil diameter to X. |
| FFO first_focus_position | First focus position of the through-focus range, along the axis from the image surface (default 0).  SPO draws its spots at this focus; SPO THF at each of the NFO positions FFO, FFO+IFO, ...  Bare FFO shows it. |
| FLOAT | Make the entrance pupil float with the stop surface's clear aperture. |
| FNO [f \| ?] | Set (and hold) the image-space F/number, or report it. |
| IFO focus_increment | Increment between through-focus positions (default 0). |
| IND | List the refractive indices of each surface at each wavelength. |
| NAO [na \| ?] | Set the object-space numerical aperture, or report it. |
| NFO number_of_focus_positions | Number of through-focus positions, 1 to 18 (default 1). |
| RSL [INF \| DEF \| radius] | OPD reference sphere: DEF (default) radius from the exit pupil for each field; INF flat reference; radius = that radius for every field (positive: sphere before the image).  Bare RSL shows the setting. |
| TIT 'text' | Set the lens title. |

## Fields & Wavelengths

| Parameter | Description |
| --------- | ----------- |
| REF n | Set the reference (control) wavelength to index n. |
| VUY\|VLY\|VUX\|VLX f1 [f2 ...]  \|  VUY Fk f | Vignetting factors (CODE V): the fraction of the entrance-pupil radius cut off at +Y (VUY), -Y (VLY), +X (VUX), -X (VLX), for fields 1..n, or for field k alone.  SET VIG computes them from the apertures. |
| WL w1 [w2 ...] | Set the system wavelengths (nm); up to 10 values. |
| WTF w1 [w2 ...] \| WTF Fk w | Set the field weights (relative; used by the SPO optimizer operand, 0 drops a field). |
| WTW s1 [s2 ...] | Set the spectral (wavelength) weights. |
| XAN a1 [a2 ...] | Set the X field angles (degrees). |
| XOB h1 [h2 ...] | Set the X object heights. |
| YAN a1 [a2 ...] | Set the Y field angles (degrees). |
| YIM h1 [h2 ...]   (XIM for X) | Set the Y paraxial image heights (field specification). |
| YOB h1 [h2 ...] | Set the Y object heights. |
| YRI h1 [h2 ...]   (XRI for X) | Set the Y real image heights (field specification). |

## Surface Parameters

| Parameter | Description |
| --------- | ----------- |
| ASP Sk | Make surface Sk an asphere type.  If in lens editor mode or when loading a lens will operate on the current surface |
| BRE Sk | Break the cemented interface at Sk: insert, ahead of Sk, a surface of the same shape (curvature, conic, asphere) with zero air space after it. |
| CUX Sk <qual> j | Sets an XZ (X-toric) curvature solve on surface Sk.  Qualifiers: AMX (aplanatic marginal, KDP APX), ACX (aplanatic chief, APCX), IMX j (angle of incidence, PIX), ICX j (PICX), UMX j (ray slope, PUX), UCX j (PUCX). |
| CUY Sk X  \|  CUY Sk <qual> j | Sets the YZ curvature (1/radius) on surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). A solve qualifier sets a curvature solve instead: AMY (aplanatic marginal, KDP APY), ACY (aplanatic chief, APCY), IMY j (angle of incidence, PIY), ICY j (PICY), UMY j (ray slope, PUY), UCY j (PUCY). |
| GLA Sk name | Sets the glass at surface Sk to the named material. Searches through available glass catalogs for the name. Use a numeric nd,vd pair (e.g. 1.5,50) to specify a model glass. |
| I Sk c1 c2 ... | Set the aspheric (4th..) coefficients of surface Sk.  $z = \dfrac{c r^2}{1+\sqrt{1-(1+k)c^2 r^2}}$. |
| INS Sk \| INS Si..j | Insert one or more new surfaces before surface k (or over the range i..j). |
| K Sk X | Set the conic constant of surface Sk to X. |
| MBF Sk d | Lens module rear vertex to rear focal point (default +MFL). |
| MCO Sk Cj value | Lens module aberration, in waves (reference wavelength) at the edge of the pupil and field: C11 spherical, C12 coma, C13 astigmatism, C14 distortion, C15 field curvature; C1-C3 axial and C6-C8 lateral colour at the MWL wavelengths. |
| MED Sk d | Lens module entrance pupil diameter.  Needed for MCO. |
| MEN Sk d | Lens module front vertex to entrance pupil (default 0). |
| MFD Sk [IMG] value | Lens module maximum field: object-space angle in degrees, or the image height with IMG.  Needed for MCO. |
| MFF Sk d | Lens module front vertex to front focal point (default -MFL). |
| MFL Sk f | Lens module focal length (required). |
| MOD Sk  \|  MOD | Make surfaces Sk and Sk+1 a lens module: a "black box" lens given by its first-order properties (MFL, MFF, MBF), perfect for an object at infinity, with the aberrations set by MCO. Both surfaces are made flat and the gap air.  MOD alone lists the modules.  CHR/ACH and finite conjugates (MRD) are not yet supported. |
| MRD Sk 0 | Lens module reduction ratio; only 0 (object at infinity) is supported so far. |
| MWL Sk w1 w2 w3 | Lens module wavelengths (nm) of the MCO colour coefficients C1-C3 and C6-C8 (default 656.27 587.56 486.13). |
| PIK XXX Sk [YYY] Sj [A [B]]  \|  PIK PRO Sk Sj [A]  \|  PIK DEC Sk Sj [A] | Pickup: parameter XXX (RDY, THI, K, A..I, GLA) of surface Sk follows parameter YYY (default XXX) of Sj, times A plus B.  PRO picks up the whole profile (curvature, conic, aspheric terms; A = -1 negates it), DEC the decenters and tilts (times A).  DEL PIK removes pickups. |
| PKL [Sk\|Si..j] | List the pickups on the given surfaces (default all). |
| RDY Sk X | Sets the radius of surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). X is the new radius value in current lens units. |
| RMD Sk REFL\|REFR\|TIR | Set the surface Sk mode: reflector, refractor (air), or TIR reflector. |
| S [rd th glass] | Advance to / add the next surface (optionally set radius, thickness, glass). |
| SI [rd th glass] | Select the image surface. |
| SLB Sk 'label' | Set a text label on surface Sk. |
| SO [rd th glass] | Select the object surface (S0). |
| SOL [Sk\|Si..j] | List the solves of surfaces Sk..j (all by default) in CODE V form. |
| SPH Sk | Make surface Sk spherical type.  If in lens edit mode, will set the current surface to spherical type |
| STO Sk | Make surface Sk the aperture stop. |
| STOP Sk | Make surface Sk the aperture stop (alias of STO). |
| THI Sk X  \|  THI Sk <qual> j | Sets the thickness on surface Sk to X. Sk can be S0 (object), S1..SN (surface by number), or Si (current surface). X is the new thickness value in current lens units. A solve qualifier sets a thickness solve instead: HMY j (paraxial marginal height, KDP PY), HCY j (chief height, PCY), HMX/HCX for XZ. |
| TIN\|CIY [Sk\|Si..j] increment | Add increment to the thickness (TIN) or Y curvature (CIY) of surface Sk (default: the current surface). |

## Apertures

| Parameter | Description |
| --------- | ----------- |
| APE [Sk\|Si..j\|SA] [F] [CLR\|EDG\|OBS] | List the apertures of surfaces Sk..j (all by default): clear apertures (CLR), edges (EDG) and obscurations (OBS) -- shape, Y and X semi-widths, decenters and rotation.  F numbers them. |
| CA YES\|NO\|APE | Whether the entered apertures are used: YES (default) they block rays, NO the ray trace ignores them, APE entered apertures only (the same as YES here).  The lens drawing always shows them. |
| CIR [Sk] r | Set a circular clear aperture of radius r on surface Sk (CIR EDG for an edge aperture). |
| CLI | Check/refresh the clear apertures. |
| ETH Sk X | Set the edge thickness aperture used by the MNE/MAE general constraints. |
| REX\|REY\|ELX\|ELY\|ADX\|ADY\|ARO [OBS] [Sk\|Si..j] value | Clear aperture (or, with OBS, obscuration) of surface Sk (default: the current surface): REX/REY rectangle and ELX/ELY ellipse X/Y semi-widths, ADX/ADY its decenter, ARO its rotation (degrees).  CIR OBS r is a circular obscuration. |

## Lens System Commands

| Parameter | Description |
| --------- | ----------- |
| BES | Find the best-focus image thickness (minimum RMS). |
| COP Sk\|Si..j Sn | Copy surfaces Sk..j of the lens and insert the copies ahead of Sn: curvature, thickness, glass, conic and aspheric terms, apertures, decenters.  Solves and pickups are not copied (they refer to other surfaces).  Copying from a library lens is not available. |
| DEL PIM \| DEL SOL <verb> Sk \| DEL CON n \| DEL VIG | Delete user-defined system data: PIM/thickness solves, angle/curvature solves (DEL SOL CUY\|CUX\|THI Sk), a constraint (DEL CON n), or vignetting. |
| FLY Si..j | Flip (reverse) the given range of surfaces. |
| RED f | Set an object-thickness reduction solve for paraxial magnification f. |
| REDO | Redo the last undone lens change. |
| SCA FAC\|OAL\|TT\|EFL\|EFY\|EFX [Si..j] [SPC] value  \|  SCA DIM I\|C\|M  \|  SCA SPC | Scale the lens, or the segment Si..j: FAC by a factor (negative flips it), OAL to that overall length (Si..Sj vertices), TT to that total track, EFL/EFY/EFX to that focal length (of the segment, with Si..j).  The specification data (EPD, object heights) is scaled too only with SPC; SCA SPC scales it by the last factor afterwards. SCA DIM I\|C\|M converts the lens and its specification data to inches, centimetres or millimetres. |
| UNDO | Undo the last lens change. |

## Optimization

| Parameter | Description |
| --------- | ----------- |
| AOI Sk [Ri] [Fk] = v \| AOI Sk [Ri] [Fk] v [w] | Angle of incidence (deg) of reference ray Ri (default R1) of field k (default F1) at Sk. AOR: angle of refraction. |
| AST3 = v \| AST3 v [w] | Third-order astigmatism sum merit entry (the AST3 SUM shown by THO). |
| ATC Sk = v \| ATC Sk v [w] | Center thickness after Sk divided by the diameter. |
| ATE Sk = v \| ATE Sk v [w] | Edge thickness after Sk divided by the diameter. |
| AUT ; <constraints/operands> ; GO | Run local optimization; define operands and constraints inside the loop. |
| AUTUI | Open the optimizer setup window (merit operands, constraints, variables). |
| AX [Wj] [Wm] = v \| AX [Wj] [Wm] v [w] | Axial colour: paraxial image distance at Wj minus at Wm (default W1 and the last wavelength). |
| CCY Sk n \| CCY Si..j n | Set the YZ-curvature variable code on surface(s). |
| CHA n ; <value> | Change entry n inside the current UPD loop. |
| CT Sk = v \| CT Sk v [w] | Center thickness after surface Sk. |
| DCON n | Delete constraint/operand number n. |
| DEL pupil_increment | AUT: the CDV ray grid spacing in pupil fractions (default 0.385, or 0.22 when the lens has aspheric surfaces).  DEL 0 restores it. |
| DIY [Fk] = v \| DIY [Fk] v [w] | Distortion fraction (0.05 = 5%) of field k, Y coordinate (default the last field). DIX for X. |
| EFL = v \| EFL v [w] | Effective focal length merit entry (constraint with =, or operand with target v, weight w). |
| EFX [Si..j] = v \| EFX [Si..j] v [w] | Effective focal length in the X-Z plane (equal to EFY: torics are not traced). |
| EFY [Si..j] = v \| EFY [Si..j] v [w] | Effective focal length of the system or of surfaces Si..j (Y-Z plane). |
| ENP = v \| ENP v [w] | Paraxial entrance pupil position from S1. |
| ERR CDV\|USR\|BTH\|CON | AUT: the error function -- CDV the CODE V error function (weighted RMS transverse ray aberration over a pupil grid, all fields and wavelengths), USR the objective terms you defined (NAME targ weight), BTH both, CON constraints only (least change of the variables).  Without ERR: your terms if any, otherwise CDV. |
| ET Sk = v \| ET Sk v [w] | Edge thickness after surface Sk, at the larger of the two semi-diameters. |
| EXP = v \| EXP v [w] | Paraxial exit pupil position from the last surface before the image. |
| FRZ | Freeze all variables (clear optimization variable codes). |
| GENCON YES\|NO | Turn the general constraints (MXT/MNT/MNE/MNA/MAE) on or off; bare GENCON queries. Set inside AUT or UPD CON. |
| GLC Sk n | Set the glass variable code on surface(s). |
| HCX [Sk] [Wm] [Zn] = v \| HCX [Sk] [Wm] [Zn] v [w] | X-Z paraxial chief ray height merit entry (default image surface, reference wavelength). |
| HCY [Sk] [Wm] [Zn] = v \| HCY [Sk] [Wm] [Zn] v [w] | Y-Z paraxial chief ray height merit entry (default image surface, reference wavelength). |
| HMX [Sk] [Wm] [Zn] = v \| HMX [Sk] [Wm] [Zn] v [w] | X-Z paraxial marginal ray height merit entry (default image surface, reference wavelength). |
| HMY [Sk] [Wm] [Zn] = v \| HMY [Sk] [Wm] [Zn] v [w] | Y-Z paraxial marginal ray height merit entry (default image surface, reference wavelength). |
| ICX [Sk] [Wm] [Zn] = v \| ICX [Sk] [Wm] [Zn] v [w] | X-Z paraxial chief ray incidence angle (index*i) merit entry (default image surface, reference wavelength). |
| ICY [Sk] [Wm] [Zn] = v \| ICY [Sk] [Wm] [Zn] v [w] | Y-Z paraxial chief ray incidence angle (index*i) merit entry (default image surface, reference wavelength). |
| IMC = v \| IMC v [w] | Image-clearance / distance merit entry. |
| IMP X | Set the optimizer improvement goal (solver accuracy, default 0.01); kept until changed or a lens is loaded. |
| IMX [Sk] [Wm] [Zn] = v \| IMX [Sk] [Wm] [Zn] v [w] | X-Z paraxial marginal ray incidence angle (index*i) merit entry (default image surface, reference wavelength). |
| IMY [Sk] [Wm] [Zn] = v \| IMY [Sk] [Wm] [Zn] v [w] | Y-Z paraxial marginal ray incidence angle (index*i) merit entry (default image surface, reference wavelength). |
| INT [Y\|N] | AUT: interactive optimization -- after every cycle, ask whether to continue (No stops with the lens at that cycle's solution). Default N.  Without a GUI the run continues (nothing to ask). |
| KC Sk n | Set the conic-constant variable code on surface(s). |
| LAT [Wj] [Wm] = v \| LAT [Wj] [Wm] v [w] | Lateral colour: paraxial chief ray image height at Wj minus at Wm. |
| LCON | List the current merit operands and constraints. |
| MAE X | Set the minimum edge air spacing (general constraint). |
| MNA X | Set the minimum axial air spacing (general constraint). |
| MNC n | AUT: the minimum number of optimization cycles (default 2, as CODE V). |
| MNE X | Set the minimum element edge thickness (general constraint). |
| MNT X | Set the minimum element center thickness (general constraint). |
| MXA = v \| MXA v [w] | Largest angle of incidence (deg) of any reference ray at any surface. |
| MXC n | AUT: the maximum number of optimization cycles (default 25). |
| MXT X | Set the maximum element center thickness (general constraint, inside AUT). |
| OAL [Si..j] = v \| OAL [Si..j] v [w] | Distance from Si to Sj (thicknesses Si..j-1; default S1 to the last surface before the image). |
| OBS fraction | AUT: CDV leaves out rays inside this fraction of the pupil radius (a central obscuration; default 0). |
| PTB = v \| PTB v [w] | Petzval-blur merit entry. |
| PTZ = v \| PTZ v [w] | Petzval-curvature merit entry (1/R of the Petzval surface, lens units^-1). |
| RAY Rk Fj x y | AUT: define user reference ray Rk (R6..R9) of field Fj at relative pupil x, y, for ray constraints (Y R6 F3 SI = 0). |
| SAP\|MER\|SAG [Y\|N] | AUT: the CDV ray pattern -- SAP the full square grid, MER only the meridional (Y) fan, SAG only the sagittal (X) fan.  N restores the default circular grid of skew rays. |
| SAS = v \| SAS v [w] | Transverse spherical aberration merit entry. |
| SD Sk = v \| SD Sk v [w] | Semi-diameter of surface Sk (assigned clear aperture, else the ray-traced extent). |
| SPO v [w] | RMS spot-size operand (weighted objective term). |
| TAR ; ... ; GO | Enter the optimization target loop (set operand targets/weights). |
| TAS = v \| TAS v [w] | Tangential-astigmatism merit entry. |
| TCO = v \| TCO v [w] | Transverse coma merit entry. |
| THC Sk n \| THC Si..j n | Set the thickness variable code on surface(s). |
| TOW ; ... ; GO | Enter the tolerancing/operand-weighting loop. |
| TT = v \| TT v [w] | Total track, object to image. |
| UCX [Sk] [Wm] [Zn] = v \| UCX [Sk] [Wm] [Zn] v [w] | X-Z paraxial chief ray exit angle merit entry (default image surface, reference wavelength). |
| UCY [Sk] [Wm] [Zn] = v \| UCY [Sk] [Wm] [Zn] v [w] | Y-Z paraxial chief ray exit angle merit entry (default image surface, reference wavelength). |
| UMX [Sk] [Wm] [Zn] = v \| UMX [Sk] [Wm] [Zn] v [w] | X-Z paraxial marginal ray exit angle merit entry (default image surface, reference wavelength). |
| UMY [Sk] [Wm] [Zn] = v \| UMY [Sk] [Wm] [Zn] v [w] | Y-Z paraxial marginal ray exit angle merit entry (default image surface, reference wavelength). |
| UPD CON | Enter an update loop to edit a data set (e.g. UPD CON for constraints). |
| VAR Sk\|Si..j | Vary every parameter of surfaces Sk..j that can vary: curvature, thickness (not the image surface's), and on conic/aspheric surfaces K and the aspheric coefficients present.  Parameters held by a solve or a pickup are skipped.  (Bare VAR is the legacy wavefront variance.) |
| VLI [IA] [Y\|N] | List the variables (free, code 0) and coupled parameters (codes 1-99) with their values. |
| WTA weight | AUT: CDV aperture weighting, w = 1 - WTA*rho^2 (default 0.5: the rim counts half as much as the centre; 0 weights all rays equally). |
| WTC weight | AUT: put the preceding = constraint into the error function with this weight instead of solving it exactly. |
| XDC\|YDC\|ZDC\|ADC\|BDC\|CDC\|XOC\|YOC\|ZOC Sk\|Si..j 0\|100 | Vary (0) or freeze (100) the decenter (XDC/YDC/ZDC), tilt (ADC/BDC/CDC) or tilt pivot point (XOC/YOC/ZOC) of surface Sk in AUT. |
| YCN [Fk] [Wm] = v \| YCN [Fk] [Wm] v [w] | Y centroid of the spot of field k (default F1) at wavelength m. XCN for X. |

## Analysis

| Parameter | Description |
| --------- | ----------- |
| CX | Print chief-ray X data. |
| CY | Print chief-ray Y data. |
| EVA name [qualifiers] | Evaluate a merit operand (with its Sk/Si..j/Wm/Fk/Ri/Zn qualifiers, as in AUT) and print its value. |
| FAB; [SUR Y] [IND Y] [SPA Y] [SAG [Sk] [max incr ref delta min]] [BES Y] [FRI Y] [SLO Y]; GO | Fabrication data: element (or surface) listing, index and spacer tables, aspheric sag tables (CODE V FAB). |
| FIO | Print the results of the first-order (paraxial) ray trace. |
| FMA; [FFD RWE\|RSD\|DST [Y]] [NFX n] [NFY n] [XMI v] [XMA v] [YMI v] [YMA v] [NRD n] [LIS Y]; GO | Field map: RMS wavefront, RMS spot diameter or distortion on a grid of field points (CODE V FMA FFD). |
| FOO; [SUR Sk\|Si..j] [BOU Y\|N] [NRD n] [RAR f] [AZI min max incr] [DAP Y\|N] [ORI x y] [LIS Y]; GO | Footprint plot: every field's beam where it crosses the designated surfaces, with the aperture outline (CODE V FOO). |
| RAYREF | Compute the per-field reference rays (R1..R5). |
| ROF [item ...] ['Fw.d'] | Ray output format for RSI and SIN: the items listed per surface -- X Y Z (intersection), L M N (direction cosines after the surface), TNX TNY (direction tangents), AOI AOR (angles of incidence and refraction, degrees), LEN (path length from the previous surface), SRL SRM SRN (surface normal) -- and the number format ('F12.8'). ROF alone restores the defaults (RSI's X Y Z XANG YANG listing; SIN lists X Y Z TNX TNY LEN). |
| RSI [Fk\|Fi..j] [Wn] XR YR  \|  RSI Fk Rs  \|  RSI [Wn] XR YR thXR thYR | Trace a single ray and list it surface by surface (see ROF). XR YR are relative pupil coordinates; without Fk the ray is on axis, without Wn at the reference wavelength (CODE V's defaults).  Rs (R1..R5) is a reference ray of field Fk, vignetting included; thXR thYR give a relative field. |
| SEIDEL ; [SUNIT LENS\|WAVES] ; GO | Seidel aberration tables (OpticStudio's Seidel Coefficients): Seidel S1-S5, CL, CT and wavefront W040..W220T coefficients (in lens units, or waves with SUNIT WAVES), transverse and longitudinal coefficients and the Petzval radius, by surface with the sum last.  Opens a data tab that updates with the lens. |
| SIN [Sk\|Si..j] [Wm] X1 Y1 tanX tanY | Trace a single ray given by its position X1 Y1 on the tangent plane of surface 1 and its direction tangents tanX tanY there (no aiming at the stop -- useful when chief rays fail).  Lists the ROF items (default X Y Z TNX TNY LEN) at surfaces Si..j (all by default), at the reference wavelength unless Wm is given. |
| SINRAYTRA ; [HX x] [HY y] [PX x] [PY y] [SETWV n] [COO DIR\|TAN] ; GO | Single ray trace (OpticStudio's): one real ray and the matching paraxial ray at normalized field (HX, HY) and pupil (PX, PY) and wavelength n, surface by surface in local coordinates, with direction cosines (COO DIR, as PRLMN) or slope tangents (COO TAN, as PRREF).  Opens a data tab that updates with the lens. Defaults HX 0 HY 1 PX 0 PY 1, reference wavelength, DIR. |
| SYSDATA | Report the system data: aperture, fields, wavelengths, units and first-order properties. |
| THO | List the third-order aberrations as CODE V does: transverse SA TCO TAS SAS PTB DST, and AX LAT (axial and lateral colour, longest to shortest wavelength) for more than one wavelength, by surface and summed.  With no off-axis field the field terms are for CODE V's default field, tan 0.02. |
| TRA; [FUL Y\|N] [NRD n]; GO | Transmission analysis (CODE V TRA): per field and wavelength, the mean transmission of the rays reaching the image -- surface losses (coatings, else bare Fresnel), bulk absorption from the glass catalogs, and both. FUL Y lists every field in full; NRD sets the pupil grid (default 20). |
| WAV; [NOM Y] [BES Y] [THR Y] [RFO Y] [NRD n] [WTF w1 ..] [CPL g1 ..]; GO | RMS wavefront error and Strehl ratio per field, at nominal and/or best composite focus, and through focus (CODE V WAV). |
| XOFF X | Set the chief-ray X offset for the wavefront calculation. |
| YOFF X | Set the chief-ray Y offset for the wavefront calculation. |
| ZRN WAV [fi] [wj] [zk] [dN] | Fit the OPD error to Zernike polynomials and print a 36-term coefficient table. |

## Plotting

| Parameter | Description |
| --------- | ----------- |
| EXPORTPNG file.png | Write the plot on the active tab to a PNG, straight from the surface it is drawn on -- i.e. exactly what is on screen, at the size the window is using.  GUI only. |
| FAN | Ray-fan plot (used within a plot loop). |
| FIE | Astigmatic field-curvature and distortion plot. |
| MTF [FRQ \| THF] [Pn] | Modulation-transfer-function plot: vs frequency at focus FFO (FRQ, the default), or at one frequency (FRE) through the focus positions FFO/IFO/NFO (THF). |
| MULTIPLOT rows cols ['title'] [Pn] | Start a page of subplots (rows x cols cells, up to 6 x 6).  Each analysis that follows (RIM, SPO, MTF, PSF, ... ; GO) fills the next cell, or the one SUBPLOT chose; a GO with no analysis open draws the page.  The page redraws when the lens changes. |
| PLOTTHO | Third-order (Seidel) aberration bar chart. |
| PLTRMS | RMS wavefront/spot vs field plot. |
| PLTSIZE [w h \| AUTO \| ?] | Set the active plot's size in pixels, or make it follow the window (AUTO). GUI only. |
| PMA | Optical-path-difference (wavefront map) plot. |
| PSF | Point-spread-function plot. |
| RAD ; [NRD n] ; [PER p1 p2 ...] ; GO | Geometric radial (encircled) energy, CODE V's RAD option: plots the energy enclosed vs radius for each field and lists the radius enclosing each percentage, about the polychromatic spot's centroid.  NRD rays across the pupil (default 20); PER up to 10 percentages (default 10, 20, .. 100). |
| RIM | Ray-aberration (rim-ray fan) plot.  WFR Y plots the wave aberration (OPD, waves) instead. |
| SPO [FLD \| THF] ; [options] ; GO | Spot diagram.  SPO / SPO FLD: one spot per field (or the chosen field), at focus FFO.  SPO THF: the chosen field and wavelength at each focus position FFO, FFO+IFO, ... (NFO of them) on one scale, in a row or a column (ORI H\|V).  The rays are moved to each focus along their slopes. |
| SUBPLOT r c [rspan cspan] | In a MULTIPLOT page: the next analysis fills the cell at row r, column c (spanning rspan rows and cspan columns). |
| VIE ; [settings] ; GO | Draw the lens layout (2D/3D system view). |
| ZRNFLD ; [SETZERNC ...] ; GO | Zernike-coefficient-vs-field plot. |

## Plot Settings

| Parameter | Description |
| --------- | ----------- |
| ADW [TIL\|FOC] x_tilt [y_tilt [focus [piston]]] | PMA: add tilt, focus (rho^2) and piston to the wavefront, in waves at the pupil edge; TIL removes the best-fit tilt first, FOC the best-fit tilt and focus.  ADW 0 turns it off. |
| AIRY ON \| OFF | Spot diagram: overlay the Airy disk on every field point. Draws a black circle of radius 1.22*lambda*F/#, using the working (image-space) F-number.  Off by default.  While it is on the plot is scaled squarely so the disk is a circle. |
| ANN RMS\|SPO [Y\|N] | SPO: annotate each spot with its RMS size (ANN RMS, on by default) and its full diameter, 100% of the rays about the centroid (ANN SPO, off by default).  ANN LEN / ANN INI are not available. |
| AST x | FIE plot: field-curvature x-axis spans -x..+x (0 = autoscale). The legacy astigmatism-table command is now ASTK. |
| ASTFLD X \| Y | Select the field direction for the astigmatism plot. |
| AZI a | Set the azimuth angle of the 3D lens view. |
| CON [SUP] [Y\|N] | PMA: contour lines over the map, numbered (CON Y) or not (CON SUP Y). |
| DIS [INT] [Y\|N] | PMA: colour map (DIS Y, the default), interferogram fringes (DIS INT Y, one fringe per wave) or none (DIS N, for contours alone). |
| DRAWSF Sk | Set the last surface drawn in the lens view. |
| DRAWSI Sk | Set the first surface drawn in the lens view. |
| DST x | FIE plot: distortion x-axis spans -x%..+x% (0 = autoscale). |
| ELEV a | Set the elevation angle of the 3D lens view. |
| FRE x | MTF THF: the spatial frequency whose modulation is plotted through focus. |
| IFR x | Set the frequency interval for the MTF plot. |
| LIS [Y\|N] | PMA, PSF, MTF: accepted for CODE V compatibility; the listing is the plot's Data tab, which is always written. |
| LSA [Y\|N\|x] | FIE: add the longitudinal spherical aberration panel (x: its +-x scale). |
| MFR x | Set the maximum frequency for the MTF plot. |
| NBR ELE Si..j | Set the surface range for the lens drawing. |
| NUMPTS n | Set the number of field samples for a vs-field plot. |
| NUMRAYS n | Set the number of rays drawn in the lens view. |
| NUMRINGS n | Spot diagram: number of rings when the tracing method is RING, 1-50 (default 20).  The pattern is evenly spaced radii with six more rays on each successive ring. |
| ORIENT ... | Set the orientation (elevation/azimuth) of the lens view. |
| PLO SUR \| BAR | PMA plot type: SUR = OPD surface map (default), BAR = bar chart of the Fringe Zernike coefficients fitted to the wavefront. |
| PLO FOC [Y\|N]  \|  PLO FRE [Y\|N] | Inside MTF: PLO FOC Y charts the modulation at one frequency (FRE) through the focus positions FFO/IFO/NFO (same as MTF THF); PLO FRE Y (or PLO FOC N) charts it against frequency, the default. The field and NRD choices made so far are kept. |
| RAN COL SPE \| RGB [r [g [b]]] | PMA: colour scale -- spectral rainbow (SPE, the default) or a ramp from black to the colour r g b (0..1 each; RGB 1 1 1 is grey). |
| RECTDENS n | Set the spot-diagram rectangular grid density for the active plot. |
| RMSDATA WAVE\|SPOT | Choose the data type for the RMS-vs-field plot. |
| RSPH [CHIEF \| NOTILT \| BEST] | Reference sphere centre for wavefront calculations. Inside a plot (e.g. PLTRMS) it sets that plot's own Reference setting, applied only while the plot computes. At the top level it sets the global default, as the legacy KDP command did.  With no qualifier it reports the current global setting. |
| SETDENS n | Set the sampling density for the active plot. |
| SETFLD n | Set the field point for the active plot (MTF, PSF, OPD). |
| SETWV n | Set the wavelength index for the active plot. |
| SETZERNC 5..9 \| 9,16,25 | Set which Zernike terms the Zernike plot shows (range or list). |
| SSI x | Set the scale of the active plot. |
| SUNIT LENS\|WAVES | Units of the SEIDEL Seidel and wavefront tables: lens units or waves at the reference wavelength. |
| TRAC RECT \| RAND \| RING | Set the spot-diagram ray-trace pattern for the active plot. |
| WFR Y\|N | RIM: plot the wave aberration (OPD in waves) instead of the transverse ray aberration. |
| ZFO Y\|N | FIE: field curves measured from the axial focus instead of the image surface. |
| ZFR n | PMA: number of Zernike terms shown in the BAR chart and Data tab, 1-37 (default 37). The fit itself is always 37 terms. |

## Tilts and Decenters

| Parameter | Description |
| --------- | ----------- |
| DAR\|BEN\|REV [Sk\|Si..j] \| RET [Sk] Sj | Decenter type of surface Sk (default: the current surface): DAR decenter-and-return, BEN decenter-and-bend (mirrors), REV reversed decenter, RET return to the coordinate system of surface Sj. DEL DDA\|DAR\|BEN\|REV Sk\|Si..j removes them. |
| GSC [Sj] | List every surface's global position (X Y Z) and orientation (ADE BDE CDE, degrees) relative to surface Sj (default S1). |
| XDE\|YDE\|ZDE\|ADE\|BDE\|CDE\|XOD\|YOD\|ZOD [Sk\|Si..j] value | Decenter (XDE/YDE/ZDE, lens units) or tilt (ADE/BDE/CDE, degrees) of the new axis at surface Sk (default: the current surface).  CODE V order: decenters first, then ADE, BDE, CDE.  XOD/YOD/ZOD: the point (relative to the surface origin) the tilt is applied about. |

## Glass Catalogs

| Parameter | Description |
| --------- | ----------- |
| CSO [catalog ...] | Sets the catalog search order: a glass named without its catalog is looked up in these catalogs first, in this order (the other catalogs follow alphabetically).  CSO alone restores the default order (LIBGLA/catalog-order.txt); CSO ? shows the current order.  The order lasts for the session. |
| GLI [catalog] | Lists the catalog glasses' refractive indices at the lens wavelengths, one line per glass ('-' outside a glass's wavelength range).  Without a catalog, every catalog in search order (CSO). |
| GPR [catalog [glass]] | Lists all data (GLI, TLI and MLI) for one glass, or for the glasses of a catalog, or of every catalog. |
| MLI [catalog] | Lists the catalog glasses' other data: glass code, nd, vd, status, density, thermal expansion, partial dispersion deviation, relative cost and the climate, stain, acid, alkali and phosphate resistance codes ('-' where the catalog gives none). |
| PRV | Starts private glass entries, which belong to the lens and are found before any catalog glass of the same name: PWL wavelength_nm ...       wavelengths for the next glasses 'label' index ...           a glass's index at each PWL wavelength END                         ends the entries One wavelength gives a constant index; more give a cubic spline over their range.  DEL PRV ALL\|'label' removes them. |
| TLI [catalog] | Lists the catalog glasses' internal transmittance data (wavelength, transmittance, sample thickness), as given in the catalog file (AGF IT lines). |

## Zoom / Multi-Configuration

There is minimal zoom support right now: it stores the commands that differentiate a configuration from the base config.

| Parameter | Description |
| --------- | ----------- |
| DEZ k | Dezoom: keep zoom position k as the (single-configuration) lens and remove the zoom data. |
| POS n | Switch the active configuration (zoom position) to n. |
| ZLI | List the zoom (multi-configuration) data; same as a bare ZOO. |
| ZOO ... \| ZOO PIM | Define zoom (multi-configuration) data. |

## Utilities

| Parameter | Description |
| --------- | ----------- |
| ! text | Comment line (ignored); used in .zoa files. |
| CAN | Cancel the open option (a plot, AUT, ...) without running it (CODE V CAN). |
| EDI PREF | Open an editor (EDI PREF opens preferences). |
| FALLBACK | Report the legacy-command fallback tally (retirement telemetry). |
| HELP\|HEL [command \| INDX \| category \| text*] | Help for the Zoa commands: HELP alone lists the categories, HELP INDX every command, HELP RDY one command's syntax and description, HELP Plotting a category's commands, HELP SP* the commands starting with SP.  The text comes from help/commands.txt, which help/gen_docs.py makes from the documentation in the source. |
| RECMACRO 1 \| RECMACRO 0 [file] \| RECMACRO | Record a macro: 1 starts recording the commands you issue; 0 file stops and saves them (macro folder if no directory, .zoa added); 0 alone asks for the file (GUI); bare reports the status. |
| SEARCHPATH [ADD folder \| REMOVE n\|folder \| CLEAR \| ?] | List or change the extra folders searched for lenses, macros and imports. |
| SET VIG\|EPD\|FNO\|NA\|NAO\|APE\|AAP\|SAP\|CAP\|ABS ... | Set data from ray tracing: SET VIG [Fk\|Fi..j] the vignetting factors from the apertures; SET EPD\|FNO\|NA\|NAO the pupil from the apertures (the largest axial beam they pass); SET APE\|AAP\|SAP [Sk\|Si..j] [EDG] [CIR\|REC\|ELL [oversize [rotation]]] apertures from the ray footprint on surfaces without one (centred, decentred, Y-decentred); SET CAP clear apertures on every surface; SET ABS Y\|N bulk absorption from the glass catalogs in the real-ray energy. |
| SUR | Surface data command. |
| TERM | Reset terminal output to the default view. |
| THREADS [n] | Number of CPU threads the CAPFN ray grid (wavefront, spot, MTF and PSF calculations) is traced with.  n = 0 is automatic (all cores), n = 1 is serial.  With no argument, reports the current setting and the number of threads that will be used.  The setting is saved as a preference.  Results do not depend on the thread count. |

## Diagnostics

| Parameter | Description |
| --------- | ----------- |
| ENGINETEST [BRIEF] | Unit test of the pure surface-placement transforms against the legacy ones.  For the current lens, runs a fixed set of rays through legacy TRNSF2 and place_into_surface at every surface NEWOBJ+1..NEWIMG, and the same points through BAKONE/FORONEL and their ports on surface NEWOBJ+1, comparing results bit for bit.  Also runs every surface's clear aperture / obscuration check (legacy CACHEK) against its port, check_apertures, over a grid of points, comparing the return code and every value CACHEK leaves behind.  Also compares the ray-aiming leaf routines (compute_aim_target/APLANA, GETZEE1, RAYDERIV, NEWDEL, MISSREF, adjustLastSurface) with their ports in mod_ray_aiming, bit for bit.  BRIEF prints only counts and verdicts. |
| RAYENGINE [ON \| OFF \| CHECK] | Chooses the tracer CAPFN and the spot diagram (SPD, SPO, and the plots built on them) use. ON (default): the global-free ray trace engine, for any lens it supports, else the legacy tracer.  OFF: always the legacy tracer.  CHECK: trace with both and compare everything they store, printing "RAYENGINE CHECK: rows N, mismatches M" (CAPFN) or "RAYENGINE CHECK (SPOT): rows N, mismatches M". The legacy RAYTRA/RAYTRA2 entry points (ray fans, the RAY family, footprints ...) follow the same setting, ray by ray; with no argument RAYENGINE reports the setting and how many of those calls the engine took, CHECK compared and declined.  RAYENGINE RESET zeroes those counters. |
| TRACECMP [n] [w] [BRIEF] | Parity check of the ray trace engine against the legacy tracer. Traces an n x n pupil grid (default 8) at wavelength slot w (default: reference wavelength) for the current lens and field with both tracers and compares status codes and the per-surface ray data.  Run FOB first.  BRIEF prints only deterministic counts and the PASS/FAIL/SKIP verdict. |
| ZINTEST | Self-check of the .zin plot-file serializer (writes and reads back every plot kind); used by the test suite. |

## Other

| Parameter | Description |
| --------- | ----------- |
| (internal) saveLensSystem | Reusable lens-save entry point used by SAV and (future) timed autosave. Writes the .zoa lens text and notifies the GUI to save the companion .zin plot file alongside it. |

