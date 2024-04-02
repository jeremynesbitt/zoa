Here is a list of new commands and their syntax.
Most of the original KDP commands still work, see here for original manual

| Parameter    | Description                                                           |
| ------------ | --------------------------------------------------------------------- |
| SAV filename | Save current lens system with filename.zoa in Projects folder         |
| RES filename | Restore filename.zoa from Projects folder                             |
| RMD Sk TYPE  | Sets surface Sk to TYPE.  Type can be REFL - mirror REFR - default    |
|              | TIR - total internal reflection surface                               | 
| VIE          | Launch viewing window (for now use UI to adjust parameters)           |
| STO Sk       | Set Sk as the stop surface (can also do this in the EDIT ui)          |
| TIT 'tittxt' | Set the title of the lens system to be tittxt                         |
| DIM K        | Set the lens units to be K.  K can be M (mm), C (cm), or I (in)       |
| RDY Sk X     | Set the radius of surface Sk to X                                     |
| INS Sk       | Inserts a surface at surface Sk                                       |
| GLA Sk nam   | Sets the glass at Surface Sk to name.  Search through each glass cat  |
| PIM          | Set a paraxial image solve on the surface before the image surface    |
| EPD X        | Sets the entrance pupil diameter to X lens units                      |
| CUY Sk X     | Sets the curvaute on surface Sk to X                                  |
| DEL Sk       | Deletes Surface Sk                                                    |
| RED X        | Adds a thickness solve on the object surface to set a parxial         |
|              | reduction factor of X                                                 | 

