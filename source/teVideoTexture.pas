unit teVideoTexture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs;


type
  {Infos about the current video frame that are exchanged between the video plugin and the player.}
  TVideoInfo = record
    Width: integer; {< Width of the video frame in memory}
    Height: Integer; {< Height of the video frame in memory}
    FPS: Double; {< Frequency the frames should change}
    PixelAspect: Double; {< Some video formats are streched, like the PAL
      16/9 Video. PixelAspect specifies the relative width of one pixel: A
      value of 1.2 eg. means that a pixel is streched to 120% if its original
      size.}
  end;
  {Pointer on TAdVideoInfo.}
  PVideoInfo = ^TVideoInfo;

  {Contains information about an audio stream.}
  TAudioInfo = record
    SampleRate: Cardinal; {< Count of samples per second.}
    BitDepth: Cardinal; {< Bits per sample.}
    Channels: Cardinal; {< Count of channels.}
  end;
  {Pointer on TAdAudioInfo.}
  PAudioInfo = ^TAudioInfo;

  {Specifies the current position of the video.}
  TVideoPosition = record
    Hour: Byte; {< Time in hours.}
    Minute: Byte; {< Time in minutes.}
    Second: Byte; {< Time in seconds.}
    Frame: Integer; {< The frame number or the sample number.}
    Timecode: double; {< The current frame position in seconds - unseperated.}
  end;

  {Current video decoding state of the video decoder plugin.}
  TMediaDecoderState = (
    vdIncomplete, {< The frame data was incomplete, we have to transfer more
      data to the video decoder.}
    vdHasFrame, {< The video decoder found a frame in the data we provided.
      It can be received by calling the "GetPacket" method.}
    vdEnd {< There was a fatal error in the video stream or it indicated that
      the video has come to an end.}
    );

   {Represents the type of an media stream that is opened using the Andorra
   Video Player interface.}
  TMediaStreamType = (
    {This stream is an video stream and contains video data.}
    amVideo,
    {This stream is an audio stream and contains audio data.}
    amAudio,
    {This stream is an stream that may contain any data (e.g. subtitles).}
    amData
  );

  {Represents an decoded media package that is recived from the decoder.}
  TMediaPacket = record
    {Type of the decoded data.}
    StreamType: TMediaStreamType;
    {Stream the packet belongs to.}
    StreamIndex: integer;
    {Position of the packet on the video timeline.}
    Timecode: TVideoPosition;
    {May contains information about the video.}
    Info: array[0..127] of Byte;
    {Size of the buffer.}
    BufferSize: integer;
    {Pointer to the buffer.}
    Buffer: PByte;
  end;

  TMediaDecoder = class;

  {Contains information about all media streams in a media file and gives the
   possibility to activate or deactivate specific streams. By default the
   first media stream with the type "amVideo" is activated.}

  { TAdMediaStream }

  TMediaStream = class
    private
      FStreamType: TMediaStreamType;
      FParent: TMediaDecoder;
      FActive: boolean;
      FIndex: integer;
      procedure SetActive(AValue: boolean);
    protected
      procedure SetActiveBySender(AValue: boolean; ASender: TMediaStream);
    public
      {Creates an instance of TAdMedia stream.}
      constructor Create(AParent: TMediaDecoder; AIndex: integer;
        AStreamType: TMediaStreamType);

      {Sepcifies whether this media stream is currently active. Is this is the
       case, the media decoder will decode this stream and send decoded data
       to the decoder thread.}
      property Active: boolean read FActive write SetActive;
      {Specifies the type of the media stream.}
      property StreamType: TMediaStreamType read FStreamType;
      {Specifies the index of the stream}
      property StreamIndex: integer read FIndex;
  end;


implementation

{ TAdMediaStream }

procedure TMediaStream.SetActive(AValue: boolean);
begin

end;

procedure TMediaStream.SetActiveBySender(AValue: boolean;
  ASender: TMediaStream);
begin

end;

constructor TMediaStream.Create(AParent: TMediaDecoder; AIndex: integer;
  AStreamType: TMediaStreamType);
begin

end;

end.

