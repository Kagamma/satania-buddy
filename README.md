## satania-buddy
Virtual Desktop Assistant in the form of Satania.

## Q & A
##### Q: Linux requirements?

A: x11, qt5, qt5pas, freetype, openssl (for IMAP)

Optional dependencies: fortune, sensors, metar

You also need a compositor, or else the app will show black screen.

##### Q: Why X11? Where's wayland support?

A: Wayland's "security" measurements makes this kind of software impossible to develope.

##### Q: OS X support when

A:![Gt999+dollars+for+a+stand+itoddlers+btfo+_954ed86b58d371154ab7f95a68054faf](https://user-images.githubusercontent.com/7451778/155552903-936f2ff1-a32b-4fe2-bbbd-0403d169808a.gif)

##### Q: Scripting API?

A: Not available right now, I am working on it.

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

##### Q: /g/ regulator?

A: Nah just a tourist.

##### Q: Why don't you contribute to existing satania-buddy projects instead of creating new one?

A: I suck at C/Rust/whatever shiny, "modern" programming languages out there. I also suck at pixel arts.

##### Q: I want to nakadashi! Where's the lewd!

A: There's limited fanservice, just enable it in `Settings`. If you want more than that, feel free to create it yourself ;)

##### Q: Uoooooooohhh?

A: Uoooooooohhh?
