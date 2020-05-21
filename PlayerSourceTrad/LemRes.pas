{$include lem_directives.inc}

unit LemRes;

interface

{$resource lemmixresources.res}             // lemmixresources contains the cursor bitmaps

{$ifdef resourcelemdata}

  //@styledef
  {$ifdef orig}
  {$resource 'data\orig\lemdata.res'}       // lemdata contains all original lemmings data
  {$endif}

  {$ifdef ohno}
  {$resource 'data\ohno\lemdata.res'}
  {$endif}

  {$ifdef h94}
  {$resource 'data\h94\lemdata.res'}       // lemdata contains all holiday94 lemmings data
  {$endif}

  {$ifdef xmas}
  {$resource 'data\xmas\lemdata.res'}       // lemdata contains all holiday94 lemmings data
  {$endif}

  {$ifdef covox}
  {$resource 'data\covox\lemdata.res'}       // lemdata contains all holiday94 lemmings data
  {$endif}

  {$ifdef prima}
  {$resource 'data\prima\lemdata.res'}       // lemdata contains all holiday94 lemmings data
  {$endif}

  {$ifdef extra}
  {$resource 'data\extra\lemdata.res'}       // lemdata contains all holiday94 lemmings data
  {$endif}

  {$ifdef cust}
  {$ifndef flexi}
    {$resource 'data\cust\lemdata.res'}
  {$else}
    {$resource 'data\flexi\lemdata.res'}
  {$endif}
  {$endif}

{$endif}


{$ifdef resourcelemsounds}
  {$resource 'sounds\win\lemsounds.res'}    // lemsounds contains the sounds (wav)
{$endif}

{$ifdef resourcelemmusic}

  //@styledef
  {$ifdef orig}
  {$resource 'music\orig\lemmusic.res'}    // lemmusic contains the music files (mod)
  {$endif}

  {$ifdef ohno}
  {$resource 'music\ohno\lemmusic.res'}    // lemmusic contains the music files (mod)
  {$endif}

  {$ifdef h94}
  {$resource 'music\h94\lemmusic.res'}    // lemmusic contains the music files (mod)
  {$endif}

  {$ifdef xmas}
  {$resource 'music\h94\lemmusic.res'}    // can just re-use H94 music as they're identical
  {$endif}

  {$ifdef covox}
  {$resource 'music\orig\lemmusic.res'}    // can just re-use Orig music as they're identical
  {$endif}

  {$ifdef prima}
  {$resource 'music\orig\lemmusic.res'}    // can just re-use Orig music as they're identical
  {$endif}

  {$ifdef extra}
  {$resource 'music\extra\lemmusic.res'}
  {$endif}

  {$ifdef cust}
  {$ifndef flexi}
    {$resource 'music\cust\lemmusic.res'}    // lemmusic contains the music files (mod)
  {$else}
    {$resource 'music\flexi\lemmusic.res'}
  {$endif}
  {$endif}

{$endif}

{$ifdef resourcebassmod}
  {$resource 'bass.res'}
{$endif}

{$ifdef resourceparticles}
  {$resource 'data\explode.res'}
{$endif}

implementation

end.

