# autocode (Clojure)

A minimal self-mutating coding agent that lives in your project. One agent loop, one built-in tool (`bash`), zero third-party dependencies (Clojure core + JDK only).

This is a Clojure-native reimagining of [empero-org/autocode](https://github.com/empero-org/autocode): immutable message maps, EDN sessions, file-based tools, and homoiconic self-modification. See `NOTICE` for upstream attribution.

## Requirements

* Java 17+ (tested on 26)
* [Clojure CLI](https://clojure.org/guides/install_clojure) (`clojure` on `PATH`)
* `bash` on `PATH`
* An OpenAI-compatible `/chat/completions` endpoint (OpenAI, OpenRouter, vLLM, Ollama, LM Studio, …)

## Quickstart

```bash
git clone https://github.com/nurazhardotcom/autocode.git
cd autocode
clojure -M -m autocode --setup   # pick base_url + model, saved to ~/.config/autocode/config.json
clojure -M -m autocode           # interactive
```

No API key in files: store it as a `$VAR` reference, e.g. `{"api_key": "$OPENAI_API_KEY"}`, or point at a local server:

```bash
AUTOCODE_BASE_URL=http://localhost:11434/v1 AUTOCODE_MODEL=qwen3-coder clojure -M -m autocode
```

## Usage

```bash
clojure -M -m autocode                         # interactive
clojure -M -m autocode "fix the failing test"  # start with a prompt
clojure -M -m autocode -p "summarize this repo" # one-shot: answer on stdout, trace on stderr
git diff | clojure -M -m autocode -p "review this"
clojure -M -m autocode -c                      # continue latest session
clojure -M -m autocode -r 20260912-091519      # resume a session
clojure -M -m autocode -m some-other-model     # override model
clojure -M -m autocode --diff                  # diff sources vs last self-edit backup
clojure -M -m autocode --reset                 # restore the backup
```

REPL commands: `/help`, `/model [name|filter]`, `/config [key [value]]`, `/config unset <key>`, `/reset`, `/compact`, `/new`, `/exit`. End a line with `\` to continue it.

## The idea

* **Small core, zero deps.** Agent loop, streaming client, sessions, compaction, and REPL in `src/autocode*.clj`, using only `clojure.core` and the JDK (`java.net.http`, `ProcessBuilder`).
* **One built-in tool: `bash`.** Commands run as written with a persistent working directory, closed stdin, and a timeout. Long output is clipped; the full text spills to `.autocode/out/` for paging.
* **The agent writes its other tools.** Any `.autocode/tools/<name>.clj` defining a `SCHEMA` map and `(run args-map)` is picked up on the next step. A tool that fails to load still shows up as `BROKEN` with its error, so the agent can fix it. See `.autocode/tools/add.clj` for the convention.
* **Self-modification.** Before each model call the agent checks whether its own sources changed. If they still parse, it backs up to `.autocode/runner.prev.clj` and relaunches into the same turn; otherwise the syntax error is reported back and the current version keeps running.
* **Compaction.** When context passes `compact_at` x `context_window` (always leaving room for a `max_tokens` reply), or the server rejects a request as too long, the model summarizes the conversation; the summary replaces everything except the newest message and latest step. Full transcripts are kept under `.autocode/sessions/old/`.
* **Minimal system prompt**, plus `AGENTS.md` (or `CLAUDE.md`) when the project has one.

## Configuration

Works with any OpenAI-compatible `/chat/completions` endpoint. Later sources win: built-in defaults, then `~/.config/autocode/config.json`, then `.autocode/config.json`, then `AUTOCODE_<KEY>` environment variables, then `-m`. `OPENAI_BASE_URL` / `OPENAI_API_KEY` are used as fallbacks.

```json
{
  "base_url": "https://openrouter.ai/api/v1",
  "api_key": "$OPENROUTER_API_KEY",
  "model": "qwen/qwen3-coder",
  "context_window": 262144,
  "compact_at": 0.8,
  "output_limit": 30000,
  "timeout": 600
}
```

## Files

```
src/autocode.clj            CLI entry, REPL, slash commands
src/autocode/agent.clj      agent loop, compaction, self-reload
src/autocode/wire.clj       streaming /chat/completions client
src/autocode/tools.clj      bash tool, clip, file-tool loader
src/autocode/tui.clj        terminal view (ANSI, plain fallback)
src/autocode/json.clj       minimal JSON reader/writer
src/autocode/config.clj     layered configuration
.autocode/tools/            tools the agent wrote (worth committing)
.autocode/sessions/         EDN transcripts (gitignored, private)
.autocode/out/              clipped tool output (gitignored, private)
.autocode/runner.prev.clj   source backup before last self-edit
```

## Safety

autocode runs `bash` as written and can rewrite its own sources — that is the product, not a bug. There is no sandbox, no permission prompt, and no dry run: a `bash` call is a real shell with your user and environment. Run it in a container, VM, or throwaway copy when the code or data matters, and keep secrets out of its environment.

## Development

```bash
clojure -M:test -m autocode.test-runner   # offline suite (localhost fake server only)
```

## License

AGPL-3.0-or-later; see [LICENSE](LICENSE). Derived from `empero-org/autocode` (Apache-2.0); upstream attribution retained in [NOTICE](NOTICE) per Apache-2.0 §4(d).
