## satania-buddy
Virtual Desktop Assistant in the form of Satania.

Download page: https://kgm.itch.io/satania-buddy

## Q & A
##### Q: Windows requirements?

A: Windows Vista or above with Aero enabled.

##### Q: Linux requirements?

A: x11, qt5, qt5pas, freetype, openssl (for IMAP), openal, vorbis

Optional dependencies: fortune, sensors, metar

You also need a compositor, or else the app will show black screen.

##### Q: Why X11? Where's wayland support?

A: Wayland's "security" measurements makes this kind of software impossible to develop. Things you just can't do in wayland:
- Ability to manually control Satania's position on screen. This is needed if we want to drag her around, or put her onto other windows.
- Can't access to other applications's window position (so no `Sit On Window`).
- Can we even be able tell wayland to make Satania's window always stay on top of everything else?

##### Q: OS X support when

A:![Gt999+dollars+for+a+stand+itoddlers+btfo+_954ed86b58d371154ab7f95a68054faf](https://user-images.githubusercontent.com/7451778/155552903-936f2ff1-a32b-4fe2-bbbd-0403d169808a.gif)

##### Q: Scripting API?

A: https://github.com/Kagamma/satania-buddy/wiki/Scripting-APIs

##### Q: How to make her speaks in my language? She only shows `?` in her chat bubble.

A: By default only ASCII is enabled. If you want to add supports for your language, make sure to add suitable fonts in `data/fonts`, enable it in `Settings`, and add unicode codepoints in Charsets.
For example this is Japanese's codepoints:
- Japanese-style punctuation (x3000 - x303f)
- Hiragana (x3040 - x309f)
- Katakana (x30a0 - x30ff)
- Full-width roman characters and half-width katakana (xff00 - xffef)
- CJK unifed ideographs - Common and uncommon kanji (x4e00 - x9faf)

##### Q: Chat features?
A: Type `>app_name` to execute external applications, for example: `>echo Hello`

Type an expression, for example `5+3*2` and she will answer with `11`

By default she has a simple rule-based chat system, you can configure patterns and responses in `data/nn/chatbot/rules.json`, then tell her to learn the rules in `Actions>Learn Rules Again` menu

##### Q: What is Chatbot in Settings?

A: She can send messages that she doesn't understand to another 3rd chatbot service if you have one available. Basically she will:
- Make a POST to chatbot's URL, with formdata format:
  ```
  message=<her message>
  ```
- Wait for chatbot server to respond in JSON format:
  ```
  {
    "type": <"chat"> or <"script">,
    "message": <"response data">
  }
  ```

##### Q: How can I change her size?

A: `Base Scaling` in `Settings`.

##### Q: How can I make her stay silent?

A: `Silent` menu.

##### Q: /g/ regulator?

A: Nah just a tourist.

##### Q: Why don't you contribute to existing satania-buddy projects instead of creating new one?

A: I suck at C/Rust/whatever shiny, "modern" programming languages out there. I also suck at pixel arts.

##### Q: I want to nakadashi! Where's the lewd!

A: There's limited fanservice, just enable it in `Settings`. If you want more than that, feel free to create it yourself ;)

##### Q: Uoooooooohhh?

A: Uoooooooohhh?
