{-$define develop}

{-------------------------------------------------------------------------------
  lemming type (make sure only one is activated!)
-------------------------------------------------------------------------------}

// Define this in the seperate project files:
// "orig", "ohno", "H93", "remake"

{-------------------------------------------------------------------------------
  resources
-------------------------------------------------------------------------------}

{$define resourcelemdata}
    // make active for including the original lemmings data as resource (dat-files)
    // when not included the program searches in its directory for the files.

{$define resourcelemsounds}
    // make active for resourcing original lemmings sounds
    // when not included no sounds are available

{$define resourcelemmusic}
    // make active for resourcing original lemmings music
    // when not included no music is available

{$define resourcebassmod}
    // make active for resourcing bassmod.dll
    // when not included the program searches for bassmod.dll in its directory.
    // if not found then music is not available

{$define resourceparticles}
   // make active to include explode.res in the exe
   // when not included the program searches in its directory for "explode.dat"

{-------------------------------------------------------------------------------
  languages
-------------------------------------------------------------------------------}

{$define language_english}
{.$define language_dutch}

{-------------------------------------------------------------------------------
  including/excluding code
-------------------------------------------------------------------------------}

{.$define vgaspecwrite}
    // make active for compiling the code for writing vgaspecbitmaps
    // (not needed for gameplaying of course)


{-------------------------------------------------------------------------------
  automatic
-------------------------------------------------------------------------------}

{$ifdef resourcelemdata}
{$define resourcelemmings}
{$endif}

{$ifdef resourcelemsounds}
{$define resourcelemmings}
{$endif}

{$ifdef resourcelemmusic}
{$define resourcelemmings}
{$endif}

{$ifdef develop}
 {$O-} // optimization
 {$R-} // range checks
 {$Q-} // overflow checks
 {$D+} // debug info
 {$L+}
 {$YD} // reference info, definitions only
 {$C+} // assertions
{$else}
 {$O+} // optimization
 {$R-} // range checks
 {$Q-} // overflow checks
 {$L-}
 {$Y-} // reference info, definitions only
 {$D-} // reference info, definitions only
 {$C-} // assertions
{$endif}


//@styledef
{$ifdef orig}
  {$define lemmingstyledefined}
{$endif}

{$ifdef ohno}
  {$define lemmingstyledefined}
{$endif}

{$ifdef h94}
  {$define lemmingstyledefined}
  {$define christmaspalette}
{$endif}

{$ifdef xmas}
  {$define lemmingstyledefined}
  {$define christmaspalette}
{$endif}

{$ifdef covox}
  {$define lemmingstyledefined}
{$endif}

{$ifdef prima}
  {$define lemmingstyledefined}
{$endif}

{$ifdef extra}
  {$define lemmingstyledefined}
{$endif}

{$ifdef cust}
  {$define lemmingstyledefined}
{$endif}

{-------------------------------------------------------------------------------
  illegal ifdef combinations
-------------------------------------------------------------------------------}

{$ifndef lemmingstyledefined}
  NO STYLE DEFINED!
{$endif}

