# Install the urbnindicators agent skill

Copies the bundled agent skill into the configuration directory of one
or more coding agents, so an agent can pull ACS data with
`urbnindicators` correctly without further prompting.

## Usage

``` r
register_urbnindicators_skill(
  agent = "claude",
  scope = c("user", "project"),
  path = NULL,
  overwrite = TRUE
)
```

## Arguments

- agent:

  Character vector of agents to install for. One or more of `"claude"`,
  `"codex"`, `"gemini"`, `"agents"` (the generic `AGENTS.md` convention,
  which also covers Cursor and other adopters), or `"all"` for every
  supported agent. Defaults to `"claude"`.

- scope:

  Either `"user"` (default) to install into the agent's per-user
  configuration directory, making the skill available in every project,
  or `"project"` to install into the current working directory so the
  skill travels with the repository.

- path:

  Optional root directory to install under, overriding the default for
  `scope` (the user's home directory, or the working directory). Mainly
  useful for testing.

- overwrite:

  Boolean. When `TRUE` (default), an existing installed copy of the
  skill is replaced. When `FALSE`, an existing installation raises an
  error.

## Value

A tibble with one row per agent, giving the `agent`, the `skill_dir` the
files were written to, and the `instructions` file updated (`NA` for
agents with native skill support), invisibly.

## Details

The skill teaches an agent the discovery workflow (never guess a
variable name), how to scope a query, that `_percent` columns are
proportions in 0-1, how margins of error work, and how to aggregate to
custom geographies.

**Agents differ in how they load instructions**, and this function
accounts for that:

- `"claude"` discovers skill directories natively. The skill is copied
  to `.claude/skills/urbnindicators/` and is loaded automatically when a
  request matches its description.

- `"codex"`, `"gemini"`, and `"agents"` read a single always-loaded
  instructions file (`AGENTS.md` or `GEMINI.md`). For these, the skill
  files are copied into the agent's configuration directory and a short
  pointer block is written into that instructions file, naming the
  trigger conditions and the path to read. Inlining the whole skill
  instead would add several thousand tokens to every unrelated
  conversation.

The pointer block is delimited by `<!-- BEGIN urbnindicators skill -->`
and `<!-- END urbnindicators skill -->`. Re-running replaces that block
and leaves the rest of the file untouched; deleting the block cleanly
unregisters the skill.

## See also

[`compile_acs_data`](https://ui-research.github.io/urbnindicators/reference/compile_acs_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## Install for Claude Code, for all projects
register_urbnindicators_skill()

## Install for every supported agent
register_urbnindicators_skill(agent = "all")

## Install into the current repository so collaborators inherit it
register_urbnindicators_skill(agent = c("claude", "codex"), scope = "project")
} # }
```
