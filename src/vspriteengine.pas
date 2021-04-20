unit vspriteengine;
{$include valkyrie.inc}
interface
uses
  Classes, SysUtils, vcolor, vgltypes, vglprogram, vtextures, vimage;

type TSpriteEngine = class;


type

{ TSpriteDataVTC }

TSpriteDataVTC = class
  constructor Create( aEngine : TSpriteEngine );
  procedure Push( Source : TSpriteDataVTC; Idx, Amount : DWord );
  procedure Push( PosID : DWord; Pos : TGLVec2i; CosColor : TColor );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; color, coscolor, light : PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0 );
  procedure PushXY( PosID, Size : DWord; Pos : TGLVec2i; Color, CosColor, Light : TColor );
  procedure Push( coord : PGLRawQCoord; tex : PGLRawQTexCoord; color, coscolor : PGLRawQColor ); // yes
  procedure Resize( newSize : DWord );
  procedure Reserve( newCapacity : DWord );
  procedure Clear;
private
  FCoords    : packed array of TGLRawQCoord;
  FTexCoords : packed array of TGLRawQTexCoord;
  FColors    : packed array of TGLRawQColor;
  FCosColors : packed array of TGLRawQColor;
  FLights    : packed array of TGLRawQColor;
  FSize      : DWord;
  FCapacity  : DWord;
  FEngine    : TSpriteEngine;
  procedure GrowTo( NewSize : DWord );
public
  property Size : DWord     read FSize;
  property Capacity : DWord read FCapacity;
end;

type

{ TSpriteShader }

TSpriteShader = class
private
  FShader              : TGLProgram;
  FShaderVertexIndex   : Integer;
  FShaderColorIndex    : Integer;
  FShaderCosColorIndex : Integer;
  FShaderLightIndex    : Integer;
  FShaderTexCoordIndex : Integer;
  FDiffuseTexLocation  : Integer;
  FCosplayTexLocation  : Integer;
public
  constructor Create( shader : TGLProgram; vertexAttrb, colorAttrb, cosAttrb, lightAttrb, texCoordAttrb : AnsiString );
  procedure Enable;
  procedure Disable;
  destructor Destroy; override;
end;

{ TSpriteDataSet }

TSpriteDataSet = class
  Normal  : TSpriteDataVTC;
  Cosplay : TSpriteDataVTC;
  Glow    : TSpriteDataVTC;
private
  FShader        : TSpriteShader;
  FDefaultShader : TSpriteShader;
public
  constructor Create( aEngine : TSpriteEngine; aCosplay, aGlow : Boolean );
  procedure Resize( Size : DWord );
  procedure Clear;
  procedure SetShader( Shader : TSpriteShader );
  procedure ResetShader;
  destructor Destroy; override;
end;

type TTextureDataSet = record
  Normal  : DWord;
  Cosplay : DWord;
  Glow    : DWord;
end;

type TTextureSet = record
  Layer      : array[1..5] of TTextureDataSet;
end;

const VSE_BG_LAYER = 1;
      VSE_FG_LAYER = 2;

type

{ TSpriteEngine }

TSpriteEngine = class
//  FTextures          : array of TTextureSet;
//  FTextureSets       : Byte;
//  FCurrentTextureSet : Byte;
  FTextureSet        : TTextureSet;
  FBlackImage        : TImage;
  FBlackTexture      : TTexture;

  FGrid              : TGLVec2i;
  FTexUnit           : TGLVec2f;
  FPos               : TGLVec2i;
  FLayers            : array[1..5] of TSpriteDataSet;
  FLayerCount        : Byte;
  FStaticLayerCount  : Byte;
  FCurrentTexture    : DWord;
  FCurrentCosTexture : DWord;
  FDefaultShader     : TSpriteShader;

  FSpriteRowCount    : Word;

  constructor Create;
  procedure Clear;
  procedure Draw;
  procedure DrawVTC( Data : TSpriteDataVTC; Shader : TSpriteShader );
  procedure DrawSet( const Data : TSpriteDataSet; const Tex : TTextureDataSet );
  // Foreground layer
  // Animation layer
  procedure SetTextures( TexID, CosID : DWord );
  destructor Destroy; override;
end;


implementation

uses
  vgl2library, math;

{ TSpriteShader }

constructor TSpriteShader.Create( shader : TGLProgram; vertexAttrb, colorAttrb, cosAttrb, lightAttrb, texCoordAttrb : AnsiString );
begin
  FShader := shader;
  FShaderVertexIndex := shader.GetAttribLocation( vertexAttrb );
  FShaderColorIndex := shader.GetAttribLocation( colorAttrb );
  FShaderCosColorIndex := shader.GetAttribLocation( cosAttrb );
  FShaderLightIndex := shader.GetAttribLocation( lightAttrb );
  FShaderTexCoordIndex := shader.GetAttribLocation( texCoordAttrb );
  FDiffuseTexLocation := shader.GetUniformLocation( 'tex' );
  FCosplayTexLocation := shader.GetUniformLocation( 'cosplay' );
end;

procedure TSpriteShader.Enable;
begin
  FShader.Bind;
  glEnableVertexAttribArray( FShaderVertexIndex );
  glEnableVertexAttribArray( FShaderTexCoordIndex );
  glEnableVertexAttribArray( FShaderColorIndex );
  glEnableVertexAttribArray( FShaderCosColorIndex );
  glEnableVertexAttribArray( FShaderLightIndex );
  glUniform1i( FDiffuseTexLocation, 0 );
  glUniform1i( FCosplayTexLocation, 1 );
end;

procedure TSpriteShader.Disable;
begin
  glDisableVertexAttribArray( FShaderVertexIndex );
  glDisableVertexAttribArray( FShaderTexCoordIndex );
  glDisableVertexAttribArray( FShaderColorIndex );
  glDisableVertexAttribArray( FShaderCosColorIndex );
  glDisableVertexAttribArray( FShaderLightIndex );
  FShader.Unbind;
end;

destructor TSpriteShader.Destroy;
begin
  FreeAndNil( FShader );
end;

{ TSpriteDataSet }

constructor TSpriteDataSet.Create( aEngine : TSpriteEngine; aCosplay, aGlow : Boolean );
begin
  Normal  := nil;
  Cosplay := nil;
  Glow    := nil;

  Normal  := TSpriteDataVTC.Create( aEngine );
  if aCosplay then Cosplay := TSpriteDataVTC.Create( aEngine );
  if aGlow    then Glow    := TSpriteDataVTC.Create( aEngine );

  FShader := aEngine.FDefaultShader;
  FDefaultShader := aEngine.FDefaultShader;
end;

procedure TSpriteDataSet.Resize( Size: DWord );
begin
  Normal.Resize( Size );
  if Cosplay <> nil then Cosplay.Resize( Size );
  if Glow    <> nil then Glow.Resize( Size );
end;

procedure TSpriteDataSet.Clear;
begin
  Normal.Clear;
  if Cosplay <> nil then Cosplay.Clear;
  if Glow    <> nil then Glow.Clear;
end;

procedure TSpriteDataSet.SetShader( Shader : TSpriteShader );
begin
  FShader := Shader;
end;

procedure TSpriteDataSet.ResetShader;
begin
  FShader := FDefaultShader;
end;

destructor TSpriteDataSet.Destroy;
begin
  FreeAndNil( Normal );
  FreeAndNil( Cosplay );
  FreeAndNil( Glow );
end;

{ TSpriteDataVTC }

constructor TSpriteDataVTC.Create( aEngine : TSpriteEngine );
begin
  FSize     := 0;
  FCapacity := 0;
  FEngine   := aEngine;
end;

procedure TSpriteDataVTC.Push( Source: TSpriteDataVTC; Idx, Amount : DWord);
begin
  if Amount + FSize > FCapacity then GrowTo( Amount + FSize );
  Move( Source.FCoords[ Idx ], FCoords[ FSize ], Amount * SizeOf(TGLRawQCoord) );
  Move( Source.FTexCoords[ Idx ], FTexCoords[ FSize ], Amount * SizeOf(TGLRawQTexCoord) );
  Move( Source.FColors[ Idx ], FColors[ FSize ], Amount * SizeOf(TGLRawQColor) );
  Move( Source.FCosColors[ Idx ], FCosColors[ FSize ], Amount * SizeOf(TGLRawQColor) );
  Move( Source.FLights[ Idx ], FLights[ FSize ], Amount * SizeOf(TGLRawQColor) );
  FSize += Amount;
end;

procedure TSpriteDataVTC.Push(PosID : DWord; Pos : TGLVec2i; CosColor : TColor);
var p1, p2     : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );

  p1 := Pos.Shifted(-1) * FEngine.FGrid;
  p2 := Pos * FEngine.FGrid;

  FCoords[ FSize ].Init( p1, p2 );

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(1) * FEngine.FTexUnit;

  FTexCoords[ FSize ].Init( t1, t2 );
  FColors[ FSize ].SetAll( TGLVec3b.Create( 255, 255, 255 ) );
  FCosColors[ FSize ].SetAll( TGLVec3b.Create( CosColor.R, CosColor.G, CosColor.B ) );
  FLights[ FSize ].SetAll( TGLVec3b.Create( 255, 255, 255 ) );

  Inc( FSize );
end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; color, coscolor, light: PGLRawQColor; TShiftX : Single = 0; TShiftY : Single = 0 );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );

  p2 := pos + FEngine.FGrid.Scaled( Size );

  FCoords[ FSize ].Init( pos, p2 );

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );
  tp += TGLVec2f.Create( TShiftX, TShiftY );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(Size) * FEngine.FTexUnit;

  FTexCoords[ FSize ].Init( t1, t2 );

  FColors[ FSize ] := color^;
  FCosColors[ FSize ] := coscolor^;
  FLights[ FSize ] := light^;
  Inc( FSize );
end;

procedure TSpriteDataVTC.PushXY(PosID, Size : DWord; Pos : TGLVec2i; Color, CosColor, Light : TColor );
var p2         : TGLVec2i;
    t1, t2, tp : TGLVec2f;
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );

  p2 := pos + FEngine.FGrid.Scaled( Size );

  FCoords[ FSize ].Init( pos, p2 );

  tp := TGLVec2f.CreateModDiv( PosID-1, FEngine.FSpriteRowCount );

  t1 := tp * FEngine.FTexUnit;
  t2 := tp.Shifted(Size) * FEngine.FTexUnit;

  FTexCoords[ FSize ].Init( t1, t2 );
  FColors[ FSize ].SetAll( TGLVec3b.Create( Color.R, Color.G, Color.B ) );
  FCosColors[ FSize ].SetAll( TGLVec3b.Create( CosColor.R, CosColor.G, CosColor.B ) );
  FLights[ FSize ].SetAll( TGLVec3b.Create( Light.R, Light.G, Light.B ) );
  Inc( FSize );
end;

procedure TSpriteDataVTC.Push(coord: PGLRawQCoord; tex: PGLRawQTexCoord; color, coscolor: PGLRawQColor);
begin
  if FSize >= FCapacity then GrowTo( Max( FCapacity * 2, 16 ) );
  FCoords[ FSize ] := coord^;
  FTexCoords[ FSize ] := tex^;
  FColors[ FSize ] := color^;
  FCosColors[ FSize ] := coscolor^;
  FLights[ FSize ].SetAll( TGLVec3b.Create( 255, 255, 255 ) );
  Inc( FSize );
end;

procedure TSpriteDataVTC.Resize( newSize: DWord );
begin
  Reserve( newSize );
  FSize := newSize;
end;

procedure TSpriteDataVTC.Reserve( newCapacity: DWord );
begin
  SetLength( FCoords, newCapacity );
  SetLength( FTexCoords, newCapacity );
  SetLength( FColors, newCapacity );
  SetLength( FCosColors, newCapacity );
  SetLength( FLights, newCapacity );
  FCapacity := newCapacity;
end;

procedure TSpriteDataVTC.Clear;
begin
  FSize := 0;
end;

procedure TSpriteDataVTC.GrowTo( NewSize: DWord );
begin
  Reserve( newSize );
end;

{ TSpriteEngine }

procedure TSpriteEngine.DrawVTC( Data : TSpriteDataVTC; Shader : TSpriteShader );
begin

  Shader.Enable;

  glVertexAttribPointer( Shader.FShaderVertexIndex, 2, GL_INT, GL_FALSE, 0, @(Data.FCoords[0]) );
  glVertexAttribPointer( Shader.FShaderTexCoordIndex, 2, GL_FLOAT, GL_FALSE, 0, @(Data.FTexCoords[0]) );
  glVertexAttribPointer( Shader.FShaderColorIndex, 3, GL_UNSIGNED_BYTE, GL_TRUE, 0, @(Data.FColors[0]) );
  glVertexAttribPointer( Shader.FShaderCosColorIndex, 3, GL_UNSIGNED_BYTE, GL_TRUE, 0, @(Data.FCosColors[0]) );
  glVertexAttribPointer( Shader.FShaderLightIndex, 3, GL_UNSIGNED_BYTE, GL_TRUE, 0, @(Data.FLights[0]) );
  glDrawArrays( GL_QUADS, 0, Data.FSize*4 );

  Shader.Disable;
end;

procedure TSpriteEngine.DrawSet(const Data: TSpriteDataSet; const Tex : TTextureDataSet);
begin

  if Data.Normal.Size > 0 then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    if (Data.Cosplay <> nil )
      then SetTextures( Tex.Normal, Tex.Cosplay )
      else SetTextures( Tex.Normal, FBlackTexture.GLTexture );
    DrawVTC( Data.Normal, Data.FShader );
  end;

  if (Data.Cosplay <> nil) and (Data.Cosplay.Size > 0) then
  begin
    glBlendFunc( GL_ONE, GL_ONE );
    SetTextures( Tex.Normal, Tex.Cosplay );
    DrawVTC( Data.Cosplay, Data.FShader );
  end;

  if (Data.Glow <> nil) and (Data.Glow.Size > 0) then
  begin
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    SetTextures( Tex.Glow, FBlackTexture.GLTexture );
    DrawVTC( Data.Glow, Data.FShader );
  end;

end;

procedure TSpriteEngine.SetTextures(TexID, CosID: DWord);
begin
  if FCurrentTexture <> TexID then
  begin
    glBindTexture( GL_TEXTURE_2D, TexID );
    FCurrentTexture := TexID;
  end;
  if FCurrentCosTexture <> CosID then
  begin
    glActiveTexture( GL_TEXTURE1 );
    glBindTexture( GL_TEXTURE_2D, CosID );
    glActiveTexture( GL_TEXTURE0 );
    FCurrentCosTexture := CosID;
  end;
end;

destructor TSpriteEngine.Destroy;
var i : Byte;
begin
  for i := 1 to High(FLayers) do
    FreeAndNil( FLayers[i] );
  FreeAndNil( FDefaultShader );
  FreeAndNil( FBlackTexture );
  FreeAndNil( FBlackImage );
end;

constructor TSpriteEngine.Create;
var i : Byte;
    iProgram : TGLProgram;
begin
  for i := 1 to High(FLayers) do
    FLayers[i] := nil;
  FSpriteRowCount    := 16;
  FGrid.Init( 32, 32 );
  FTexUnit.Init( 1.0 / FSpriteRowCount, 1.0 / 32 );
  FPos.Init(0,0);
  FCurrentTexture    := 0;
  FLayerCount        := 0;
  FStaticLayerCount  := 0;

  FBlackImage := TImage.Create( 1, 1 );
  FBlackImage.Fill( ColorBlack );
  FBlackTexture := TTexture.Create( FBlackImage, False );

  iProgram := TGLProgram.Create(
    'in vec4 color;'#10 +
    'in vec4 coscolor;'#10 +
    'in vec4 light;'#10 +
    'in vec4 vertex;'#10 +
    'in vec4 texcoord;'#10 +
    'out varying vec4 ex_color;'#10 +
    'out varying vec4 ex_coscolor;'#10 +
    'out varying vec4 ex_light;'#10 +
    'out varying vec4 ex_texcoord;'#10 +
    'void main()'#10 +
    '{'#10 +
    '  gl_Position = gl_ModelViewProjectionMatrix * vertex;'#10 +
    '  ex_texcoord = texcoord;'#10 +
    '  ex_color = color;'#10 +
    '  ex_coscolor = coscolor;'#10 +
    '  ex_light = light;'#10 +
    '}'#10,
    'uniform sampler2D tex;'#10 +
    'uniform sampler2D cosplay;'#10 +
    'in vec4 ex_color;'#10 +
    'in vec4 ex_coscolor;'#10 +
    'in vec4 ex_light;'#10 +
    'in vec4 ex_texcoord;'#10 +
    'void main()'#10 +
    '{'#10 +
    '  vec4 diffuse = texture2D(tex, ex_texcoord.st) * ex_color;'#10 +
    '  diffuse.rgb += texture2D(cosplay, ex_texcoord.st).rgb * ex_coscolor.rgb;'#10 +
    '  gl_FragColor = diffuse * ex_light;'#10 +
    '}'#10 );

  FDefaultShader := TSpriteShader.Create( iProgram, 'vertex', 'color', 'coscolor', 'light', 'texcoord' );
end;

procedure TSpriteEngine.Clear;
var i : Byte;
begin
  if FLayerCount > 0 then
  for i := 1 to FLayerCount do
    FLayers[ i ].Clear;
end;

procedure TSpriteEngine.Draw;
var i : Byte;
begin
  FCurrentTexture := 0;
  FCurrentCosTexture := 0;
  glTranslatef( -FPos.X, -FPos.Y, 0.0 );

  glActiveTexture( GL_TEXTURE1 );
  glEnable( GL_TEXTURE_2D );
  glActiveTexture( GL_TEXTURE0 );
  glEnable( GL_TEXTURE_2D );

  glDisable( GL_DEPTH_TEST );
  glEnable( GL_BLEND );
  glColor4f( 1.0, 1.0, 1.0, 1.0 );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  if FLayerCount > 0 then
  for i := 1 to FLayerCount do
    DrawSet( FLayers[ i ], FTextureSet.Layer[ i ] );

  glActiveTexture( GL_TEXTURE1 );
  glDisable( GL_TEXTURE_2D );
  glActiveTexture( GL_TEXTURE0 );

  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glTranslatef( FPos.X, FPos.Y, 0.0 );
end;

initialization

  Assert( SizeOf( Integer ) = SizeOf( GLInt ) );
  Assert( SizeOf( Single )  = SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQCoord )    = 8 * SizeOf( GLInt ) );
  Assert( SizeOf( TGLRawQTexCoord ) = 8 * SizeOf( GLFloat ) );
  Assert( SizeOf( TGLRawQColor )    = 12 * SizeOf( GLByte ) );

end.

