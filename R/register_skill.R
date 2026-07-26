####----AGENT SKILL INSTALLATION----####

## Markers delimiting the pointer block this package writes into an agent's
## instructions file. Re-registering replaces the block between them rather than
## appending a second copy.
.skill_block_begin = "<!-- BEGIN urbnindicators skill -->"
.skill_block_end = "<!-- END urbnindicators skill -->"

## Supported agent identifiers, in the order they are installed.
.supported_agents = c("claude", "codex", "gemini", "agents")

## Resolve the install target for one agent.
##
## `skill_dir` is where the SKILL.md tree is copied. `instructions` is the
## agent's always-loaded instructions file, or NULL when the agent discovers
## skill directories natively (Claude). `label` is used in messages.
skill_target = function(agent, scope, root) {
  ## per-agent config directory, identical under both scopes: `~/.claude` for
  ## user scope, `<project>/.claude` for project scope
  config_dir = function(name) file.path(root, name)

  ## at user scope the instructions file lives inside the agent's config
  ## directory; at project scope agents read it from the repository root
  instructions_path = function(name, file) {
    if (scope == "user") file.path(config_dir(name), file) else file.path(root, file)
  }

  switch(
    agent,
    claude = list(
      label = "Claude Code / Claude Desktop",
      skill_dir = file.path(config_dir(".claude"), "skills", "urbnindicators"),
      instructions = NULL),
    codex = list(
      label = "OpenAI Codex",
      skill_dir = file.path(config_dir(".codex"), "skills", "urbnindicators"),
      instructions = instructions_path(".codex", "AGENTS.md")),
    gemini = list(
      label = "Google Gemini CLI",
      skill_dir = file.path(config_dir(".gemini"), "skills", "urbnindicators"),
      instructions = instructions_path(".gemini", "GEMINI.md")),
    agents = list(
      label = "AGENTS.md-compatible agents",
      skill_dir = file.path(config_dir(".agents"), "skills", "urbnindicators"),
      instructions = instructions_path(".agents", "AGENTS.md")))
}

## Copy the bundled skill tree to `destination`.
install_skill_files = function(source_dir, destination, overwrite) {
  if (dir.exists(destination) && !overwrite) {
    cli::cli_abort(c(
      "A skill is already installed at {.path {destination}}.",
      "i" = "Pass {.code overwrite = TRUE} to replace it."))
  }

  ## remove first so files deleted upstream do not linger from a prior install
  if (dir.exists(destination)) unlink(destination, recursive = TRUE)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  copied = file.copy(source_dir, dirname(destination), recursive = TRUE)
  if (!all(copied) || !file.exists(file.path(destination, "SKILL.md"))) {
    cli::cli_abort("Failed to copy the skill files to {.path {destination}}.")
  }

  invisible(destination)
}

## The pointer block written into a non-Claude agent's instructions file.
##
## Deliberately short: it names the trigger conditions and the path to read, so
## the agent loads the full skill only when a task actually calls for it. The
## alternative -- inlining the skill -- would put several thousand tokens of ACS
## material into every unrelated conversation.
skill_pointer_block = function(skill_dir) {
  c(
    .skill_block_begin,
    "",
    "## urbnindicators (American Community Survey data in R)",
    "",
    paste0(
      "When a task involves ACS or Census estimates in R -- demographics, ",
      "housing, income, poverty, employment, or commuting for US tracts, ",
      "counties, states, or block groups -- or mentions the urbnindicators ",
      "package, `compile_acs_data()`, `interpolate_acs()`, a raw ACS table ",
      "code such as B25070, margins of error, or a variable codebook, read ",
      "the skill at:"),
    "",
    paste0("    ", file.path(skill_dir, "SKILL.md")),
    "",
    paste0(
      "It carries the required discovery workflow (never guess a variable ",
      "name), query scoping rules, output conventions (percentages are ",
      "proportions in 0-1), and margin-of-error discipline. Deeper reference ",
      "material sits in the adjacent `references/` directory; read those files ",
      "only when the task needs them."),
    "",
    .skill_block_end)
}

## Insert or replace the pointer block in an agent's instructions file.
## Content outside the markers is preserved verbatim.
write_pointer_block = function(instructions_path, skill_dir) {
  block = skill_pointer_block(skill_dir)
  dir.create(dirname(instructions_path), recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(instructions_path)) {
    writeLines(block, instructions_path)
    return(invisible("created"))
  }

  existing = readLines(instructions_path, warn = FALSE)
  begins = which(existing == .skill_block_begin)
  ends = which(existing == .skill_block_end)

  if (length(begins) == 0 || length(ends) == 0) {
    ## no managed block yet -- append, keeping a blank line as separation
    writeLines(c(existing, "", block), instructions_path)
    return(invisible("appended"))
  }

  ## replace from the first marker to the last, so a malformed or duplicated
  ## block collapses back to a single managed section
  first_begin = begins[1]
  last_end = ends[length(ends)]

  before = if (first_begin > 1) existing[seq_len(first_begin - 1)] else character(0)
  after = if (last_end < length(existing)) {
    existing[seq(last_end + 1, length(existing))]
  } else {
    character(0)
  }

  writeLines(c(before, block, after), instructions_path)
  invisible("updated")
}

#' @title Install the urbnindicators agent skill
#' @description Copies the bundled agent skill into the configuration directory
#'    of one or more coding agents, so an agent can pull ACS data with
#'    \code{urbnindicators} correctly without further prompting.
#' @details
#'    The skill teaches an agent the discovery workflow (never guess a variable
#'    name), how to scope a query, that \code{_percent} columns are proportions
#'    in 0-1, how margins of error work, and how to aggregate to custom
#'    geographies.
#'
#'    **Agents differ in how they load instructions**, and this function
#'    accounts for that:
#'    \itemize{
#'      \item \code{"claude"} discovers skill directories natively. The skill is
#'        copied to \code{.claude/skills/urbnindicators/} and is loaded
#'        automatically when a request matches its description.
#'      \item \code{"codex"}, \code{"gemini"}, and \code{"agents"} read a single
#'        always-loaded instructions file (\code{AGENTS.md} or
#'        \code{GEMINI.md}). For these, the skill files are copied into the
#'        agent's configuration directory and a short pointer block is written
#'        into that instructions file, naming the trigger conditions and the
#'        path to read. Inlining the whole skill instead would add several
#'        thousand tokens to every unrelated conversation.
#'    }
#'
#'    The pointer block is delimited by
#'    \code{<!-- BEGIN urbnindicators skill -->} and
#'    \code{<!-- END urbnindicators skill -->}. Re-running replaces that block
#'    and leaves the rest of the file untouched; deleting the block cleanly
#'    unregisters the skill.
#' @param agent Character vector of agents to install for. One or more of
#'    \code{"claude"}, \code{"codex"}, \code{"gemini"}, \code{"agents"} (the
#'    generic \code{AGENTS.md} convention, which also covers Cursor and other
#'    adopters), or \code{"all"} for every supported agent. Defaults to
#'    \code{"claude"}.
#' @param scope Either \code{"user"} (default) to install into the agent's
#'    per-user configuration directory, making the skill available in every
#'    project, or \code{"project"} to install into the current working
#'    directory so the skill travels with the repository.
#' @param path Optional root directory to install under, overriding the default
#'    for \code{scope} (the user's home directory, or the working directory).
#'    Mainly useful for testing.
#' @param overwrite Boolean. When \code{TRUE} (default), an existing installed
#'    copy of the skill is replaced. When \code{FALSE}, an existing installation
#'    raises an error.
#' @returns A tibble with one row per agent, giving the \code{agent}, the
#'    \code{skill_dir} the files were written to, and the
#'    \code{instructions} file updated (\code{NA} for agents with native skill
#'    support), invisibly.
#' @seealso \code{\link{compile_acs_data}}
#' @examples
#' \dontrun{
#' ## Install for Claude Code, for all projects
#' register_urbnindicators_skill()
#'
#' ## Install for every supported agent
#' register_urbnindicators_skill(agent = "all")
#'
#' ## Install into the current repository so collaborators inherit it
#' register_urbnindicators_skill(agent = c("claude", "codex"), scope = "project")
#' }
#' @export
register_urbnindicators_skill = function(
    agent = "claude",
    scope = c("user", "project"),
    path = NULL,
    overwrite = TRUE) {

  scope = match.arg(scope)

  if (!is.character(agent) || length(agent) == 0) {
    cli::cli_abort("{.arg agent} must be a character vector of agent names.")
  }
  if (any(agent == "all")) agent = .supported_agents
  agent = unique(agent)

  ## bound to a local name because cli treats a `{.foo}` interpolation as a
  ## style, so `{.supported_agents}` would not resolve as a variable
  supported = .supported_agents
  unknown = setdiff(agent, supported)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown agent{?s}: {.val {unknown}}.",
      "i" = "Supported: {.val {supported}}, or {.val all}."))
  }

  if (!rlang::is_bool(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be TRUE or FALSE.")
  }

  source_dir = system.file("skills", "urbnindicators", package = "urbnindicators")
  if (!nzchar(source_dir) || !file.exists(file.path(source_dir, "SKILL.md"))) {
    cli::cli_abort(c(
      "Could not locate the bundled skill files.",
      "i" = "Reinstall the package: {.code renv::install(\"UI-Research/urbnindicators\")}."))
  }

  root = if (!is.null(path)) {
    path.expand(path)
  } else if (scope == "user") {
    path.expand("~")
  } else {
    getwd()
  }

  specs = purrr::map(agent, skill_target, scope = scope, root = root)

  ## pre-flight: fail before writing anything, so a multi-agent install does
  ## not leave some agents registered and others not
  if (!overwrite) {
    existing = purrr::keep(specs, ~ dir.exists(.x[["skill_dir"]]))
    n_existing = length(existing)
    if (n_existing > 0) {
      cli::cli_abort(c(
        "A skill is already installed for {n_existing} of the requested agent{?s}.",
        stats::setNames(
          purrr::map_chr(existing, ~ .x[["skill_dir"]]),
          rep("*", n_existing)),
        ## each cli bullet is formatted independently, so the pluralizing
        ## quantity has to be restated here
        "i" = "Pass {.code overwrite = TRUE} to replace {cli::qty(n_existing)}{?it/them}."))
    }
  }

  results = purrr::map2(agent, specs, function(this_agent, spec) {
    install_skill_files(source_dir, spec[["skill_dir"]], overwrite)

    if (is.null(spec[["instructions"]])) {
      cli::cli_inform(c(
        "v" = "{spec[['label']]}: installed to {.path {spec[['skill_dir']]}}.",
        "i" = "Loaded automatically when a request involves ACS data."))
      instructions = NA_character_
    } else {
      action = write_pointer_block(spec[["instructions"]], spec[["skill_dir"]])
      cli::cli_inform(c(
        "v" = "{spec[['label']]}: installed to {.path {spec[['skill_dir']]}}.",
        "i" = "Pointer block {action} in {.path {spec[['instructions']]}}."))
      instructions = spec[["instructions"]]
    }

    tibble::tibble(
      agent = this_agent,
      skill_dir = spec[["skill_dir"]],
      instructions = instructions)
  })

  invisible(purrr::list_rbind(results))
}
