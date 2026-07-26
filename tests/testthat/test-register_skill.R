# Tests for register_urbnindicators_skill() (R/register_skill.R). These are
# pure filesystem operations -- no network or API key needed. Every test
# installs under a withr-managed temporary root via the `path` argument, so
# nothing touches the user's real ~/.claude, ~/.codex, or ~/.gemini.

skill_files = function(skill_dir) {
  sort(list.files(skill_dir, recursive = TRUE))
}

expected_skill_files = c(
  "SKILL.md",
  "references/custom-variables.md",
  "references/geographies-and-interpolation.md",
  "references/troubleshooting.md")

test_that("installs the full skill tree for Claude", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(agent = "claude", scope = "user", path = root))

  expect_equal(nrow(result), 1)
  expect_equal(result$agent, "claude")
  expect_equal(
    result$skill_dir,
    file.path(root, ".claude", "skills", "urbnindicators"))
  expect_equal(skill_files(result$skill_dir), sort(expected_skill_files))
})

test_that("Claude gets no pointer block (native skill discovery)", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(agent = "claude", scope = "user", path = root))

  expect_true(is.na(result$instructions))
  expect_false(file.exists(file.path(root, "AGENTS.md")))
  expect_false(file.exists(file.path(root, ".claude", "AGENTS.md")))
})

test_that("agents without native skill support get a pointer block", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(
      agent = c("codex", "gemini"), scope = "user", path = root))

  expect_equal(result$agent, c("codex", "gemini"))
  expect_equal(result$instructions, c(
    file.path(root, ".codex", "AGENTS.md"),
    file.path(root, ".gemini", "GEMINI.md")))

  purrr::walk2(result$instructions, result$skill_dir, function(file, skill_dir) {
    contents = readLines(file)
    expect_true("<!-- BEGIN urbnindicators skill -->" %in% contents)
    expect_true("<!-- END urbnindicators skill -->" %in% contents)
    ## the block must point at that agent's own installed copy
    expect_true(any(grepl(file.path(skill_dir, "SKILL.md"), contents, fixed = TRUE)))
  })
})

test_that("`all` installs every supported agent", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(agent = "all", scope = "user", path = root))

  expect_equal(result$agent, c("claude", "codex", "gemini", "agents"))
  purrr::walk(result$skill_dir, function(dir) {
    expect_equal(skill_files(dir), sort(expected_skill_files))
  })
})

test_that("project scope writes instructions to the project root", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(agent = "codex", scope = "project", path = root))

  expect_equal(result$instructions, file.path(root, "AGENTS.md"))
  expect_true(file.exists(file.path(root, "AGENTS.md")))
  expect_equal(
    result$skill_dir,
    file.path(root, ".codex", "skills", "urbnindicators"))
})

test_that("re-registering is idempotent and preserves surrounding content", {
  root = withr::local_tempdir()
  instructions = file.path(root, "AGENTS.md")
  writeLines(c("# House rules", "", "Prefer tabs.", "", "## Notes", "keep me"), instructions)

  register = function() {
    suppressMessages(
      register_urbnindicators_skill(agent = "agents", scope = "project", path = root))
  }
  register()
  after_first = readLines(instructions)
  register()
  register()
  after_third = readLines(instructions)

  expect_identical(after_first, after_third)
  expect_equal(sum(after_third == "<!-- BEGIN urbnindicators skill -->"), 1)
  expect_true(all(c("# House rules", "Prefer tabs.", "keep me") %in% after_third))
})

test_that("a stale block is replaced in place, keeping content on both sides", {
  root = withr::local_tempdir()
  instructions = file.path(root, "AGENTS.md")
  writeLines(
    c("before",
      "<!-- BEGIN urbnindicators skill -->",
      "STALE",
      "<!-- END urbnindicators skill -->",
      "after"),
    instructions)

  suppressMessages(
    register_urbnindicators_skill(agent = "agents", scope = "project", path = root))
  contents = readLines(instructions)

  expect_false("STALE" %in% contents)
  expect_true("before" %in% contents)
  expect_true("after" %in% contents)
  expect_equal(sum(contents == "<!-- BEGIN urbnindicators skill -->"), 1)
})

test_that("reinstalling drops files removed upstream", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(agent = "claude", scope = "user", path = root))

  stray = file.path(result$skill_dir, "references", "obsolete.md")
  writeLines("leftover", stray)
  expect_true(file.exists(stray))

  suppressMessages(
    register_urbnindicators_skill(agent = "claude", scope = "user", path = root))
  expect_false(file.exists(stray))
})

test_that("overwrite = FALSE aborts before writing anything", {
  root = withr::local_tempdir()
  suppressMessages(
    register_urbnindicators_skill(agent = "claude", scope = "user", path = root))

  expect_error(
    register_urbnindicators_skill(
      agent = c("claude", "codex"), scope = "user", path = root, overwrite = FALSE),
    "already installed")

  ## codex was requested second; the pre-flight check must stop it being
  ## installed when an earlier agent already exists
  expect_false(dir.exists(file.path(root, ".codex")))
})

test_that("overwrite = FALSE succeeds when nothing is installed", {
  root = withr::local_tempdir()
  expect_no_error(
    suppressMessages(
      register_urbnindicators_skill(
        agent = "claude", scope = "user", path = root, overwrite = FALSE)))
})

test_that("invalid arguments are rejected", {
  root = withr::local_tempdir()

  expect_error(
    register_urbnindicators_skill(agent = "copilot", path = root),
    "Unknown agent")
  expect_error(
    register_urbnindicators_skill(agent = character(0), path = root),
    "character vector")
  expect_error(
    register_urbnindicators_skill(agent = "claude", path = root, overwrite = "yes"),
    "must be TRUE or FALSE")
  expect_error(
    register_urbnindicators_skill(agent = "claude", scope = "global", path = root))
})

test_that("duplicate agent names are installed once", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(
      agent = c("claude", "claude"), scope = "user", path = root))

  expect_equal(nrow(result), 1)
})

test_that("the installed skill is the one bundled with the package", {
  root = withr::local_tempdir()
  result = suppressMessages(
    register_urbnindicators_skill(agent = "claude", scope = "user", path = root))

  source_dir = system.file("skills", "urbnindicators", package = "urbnindicators")
  skip_if(!nzchar(source_dir), "bundled skill not found (not an installed package)")

  expect_identical(
    readLines(file.path(result$skill_dir, "SKILL.md")),
    readLines(file.path(source_dir, "SKILL.md")))
})
