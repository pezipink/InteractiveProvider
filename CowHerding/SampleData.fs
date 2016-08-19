
[<AutoOpen>]
module Data 
    open FSharp.Data

    [<Literal>]
    let private sample =
        """<SokobanLevels xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="SokobanLev.xsd">
          <Title>Original &amp; Extra</Title>
          <Description>
        The 50 original levels from Sokoban plus the 40 from Extra.
          </Description>
          <LevelCollection Copyright="Thinking Rabbit" MaxWidth="20" MaxHeight="17">
            <Level Id="d1" Width="19" Height="11">
              <L>    #####</L>
              <L>    #   #</L>
              <L>    #$  #</L>
              <L>  ###  $##</L>
              <L>  #  $ $ #</L>
              <L>### # ## #   ######</L>
              <L>#   # ## #####  ..#</L>
              <L># $  $          ..#</L>
              <L>##### ### #@##  ..#</L>
              <L>    #     #########</L>
              <L>    #######</L>
            </Level>
            <Level Id="2b" Width="14" Height="10">
              <L>############</L>
              <L>#..  #     ###</L>
              <L>#..  # $  $  #</L>
              <L>#..  #$####  #</L>
              <L>#..    @ ##  #</L>
              <L>#..  # #  $ ##</L>
              <L>###### ##$ $ #</L>
              <L>  # $  $ $ $ #</L>
              <L>  #    #     #</L>
              <L>  ############</L>
            </Level>
          </LevelCollection>
        </SokobanLevels>"""

    type CowLevel = XmlProvider<sample>

    type DonNorris = JsonProvider<"""{ "type": "success", "value": { "id": 268, "joke": "Time waits for no man. Unless that man is Chuck Norris." } }""" >
