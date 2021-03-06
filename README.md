## satania-buddy
Virtual Desktop Assistant in the form of Satania.

Download page: https://kgm.itch.io/satania-buddy

## Q & A

##### Q: Overall system requirements?

A:
- CPU: Any decent x86_64 CPU should be able to run. I personally run it on an i7-6700HQ and it barely reach 1% CPU usage (around 2% - 3% with Speech Recognition enable).
- RAM: The app consumes 100MB without Speech Recognition enable, and 300MB with Speech Recognition enable.
- GPU: Any decent intel iGPU with support for OpenGL 2.0 should be enough to run the app.

##### Q: Windows requirements?

A: Windows Vista or above with Aero enabled.

##### Q: Linux requirements?

A: x11, qt5, qt5pas, freetype, openssl, openal, vorbis

Optional dependencies: fortune, sensors

You also need a compositor, or else the app will show black screen.

##### Q: Why X11? Where's wayland support?

A: Wayland's "security" measurements makes this kind of software impossible to develop. Things you just can't do in wayland:
- Ability to manually control Satania's position on screen. This is needed if we want to drag her around, move her to other monitors/virtual desktops, or put her onto other windows.
- Can't access to other applications's window position (so no `Sit On Window`).
- Can we even be able to tell wayland to make Satania's window always stay on top of everything else?

##### Q: OS X support when

A:![Gt999+dollars+for+a+stand+itoddlers+btfo+_954ed86b58d371154ab7f95a68054faf](https://user-images.githubusercontent.com/7451778/155552903-936f2ff1-a32b-4fe2-bbbd-0403d169808a.gif)\
_(Joking aside OS X has a much higher chance to get ported over compare to Linux's wayland)_

##### Q: Privacy?

A: Satania Buddy works completely offline without any connections to remote servers.

##### Q: Scripting API?

A: https://github.com/Kagamma/satania-buddy/wiki/Scripting-References-&-APIs

##### Q: How to make her speaks in my language? She only shows `_` in her chat bubble.

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

By default she has a simple rule-based chat system, you can configure patterns and responses in `Rules`, then tell her to learn the rules by pressing the `Learn` button.

If none of the above apply, then she will send your messages to https://www.wolframalpha.com/ for potential answers. Make sure to set your own AppID in `Chatbot > WolframAlpha`.

##### Q: What is Chatbot > Custom Server in Settings?

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
  By setting a custom server, she will ignore WolframAlpha setting and send your messages to custom server instead.

##### Q: How can I change her size?

A: `Base Scaling` in `Settings`.

##### Q: How can I make her stay silent?

A: `Silent` menu.

##### Q: Why don't you contribute to existing satania-buddy projects instead of creating new one?

A: I suck at C/Rust/whatever shiny, "modern" programming languages out there. I also suck at pixel arts.

##### Q: Uoooooooohhh?

A: Uoooooooohhh?
